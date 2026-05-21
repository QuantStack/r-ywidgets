# yr::Origin wraps a Rust externalptr. Package-level bindings get serialised
# by R's lazy-load DB, which writes extptrs as NULL — restoring them on next
# session triggers "expected non-null pointer in externalptr". Build them in
# .onLoad so each R session gets fresh pointers.
LOCAL_ORIGIN <- NULL
REMOTE_ORIGIN <- NULL

.onLoad <- function(libname, pkgname) {
  LOCAL_ORIGIN <<- yr::Origin$new(0L)
  REMOTE_ORIGIN <<- yr::Origin$new(1L)
}

# Coerce arbitrary R values to a yr::Prelim, wrapping non-Prelims as Any.
.as_prelim <- function(value) {
  if (inherits(value, "Prelim")) value else yr::Prelim$any(value)
}

#' Shared base for `YAttrWidget` and `YRootWidget`
#'
#' Owns the `yr::Doc` and the per-attribute storage registry, and provides
#' the common `connect(name = fn, ...)` subscription method. Subclasses
#' override `register_storage()` to materialize their flavor of storage.
#'
#' @export
WidgetBase <- R6::R6Class(
  "WidgetBase",
  public = list(
    #' @field ydoc The underlying `yr::Doc`.
    ydoc = NULL,

    #' @description Create a new `WidgetBase`.
    #' @param ydoc An existing `yr::Doc` to adopt, or `NULL` to create a fresh one.
    initialize = function(ydoc = NULL) {
      self$ydoc <- if (is.null(ydoc)) yr::Doc$new() else ydoc
      private$.storages <- list()
    },

    #' @description Subscribe callbacks to attribute changes by name. Each
    #'   callback fires on both local writes and remote-peer updates (root-
    #'   backed attributes do not emit local-changed signals).
    #' @param ... Named callbacks: `attr_name = function(value_or_event) {}`.
    #' @return `invisible(self)`.
    connect = function(...) {
      args <- list(...)
      for (nm in names(args)) {
        r <- private[[paste0(".", nm)]]
        r$local_changed$connect(args[[nm]])
        r$remote_changed$connect(args[[nm]])
      }
      invisible(self)
    },

    #' @description Abstract — overridden by subclasses to materialize a
    #'   storage backend for one attribute and register it under `name`.
    #' @param name Attribute name.
    #' @param ... Subclass-specific arguments (e.g. `default` or `prelim`).
    register_storage = function(name, ...) {
      stop("register_storage() must be implemented by a subclass.")
    }
  ),

  private = list(
    .storages = NULL
  )
)

#' `Reactive` storage backed by a Y.js `attrs` map entry.
#'
#' Implements the same protocol as [RemoteLocalStorage] (`read()`, `update()`,
#' and a public `remote_changed` [Signal]). Writes via `update()` use
#' `LOCAL_ORIGIN`; the owning [YAttrWidget] runs the `_attrs` observer and emits on
#' `remote_changed` when a peer changes this key.
#'
#' @export
YAttrStorage <- R6::R6Class(
  "YAttrStorage",
  private = list(
    ydoc = NULL,
    attrs = "_attrs",
    key = NULL
  ),
  public = list(
    #' @field remote_changed [Signal] fired on remote changes to the value.
    remote_changed = NULL,

    #' @description Bind the backend to one `attrs` key. If `default` is
    #'   non-NULL and the key is currently absent, `default` is written under
    #'   it (a non-Prelim is wrapped as `yr::Prelim$any`).
    #' @param ydoc The `yr::Doc`.
    #' @param attrs Its attribute map.
    #' @param key Attribute key to read/write.
    #' @param default Default value (Prelim or any R value), or `NULL` to
    #'   skip the initial write entirely.
    initialize = function(ydoc, attrs, key, default = NULL) {
      private$ydoc <- ydoc
      private$attrs <- attrs
      private$key <- key
      self$remote_changed <- Signal$new()

      # If default is given initialize it when not present in the ydoc.
      # It may already be present if joining another widget.
      if (!is.null(default)) {
        prelim_default <- .as_prelim(default)
        private$ydoc$with_transaction(
          function(trans) {
            if (is.null(private$attrs$get(trans, private$key))) {
              private$attrs$insert(trans, private$key, prelim_default)
            }
          },
          mutable = TRUE,
          origin = LOCAL_ORIGIN
        )
      }
    },

    #' @description Return the value stored under `key`.
    read = function() {
      private$ydoc$with_transaction(
        function(trans) private$attrs$get(trans, private$key)
      )
    },

    #' @description Write `value` under `key`; no-op when unchanged.
    #' @param value New value.
    #' @return `TRUE` iff the value was written.
    update = function(value) {
      private$ydoc$with_transaction(
        function(trans) {
          if (identical(private$attrs$get(trans, private$key), value)) {
            return(FALSE)
          }
          private$attrs$insert(trans, private$key, .as_prelim(value))
          TRUE
        },
        mutable = TRUE,
        origin = LOCAL_ORIGIN
      )
    }
  )
)


#' Base class for CRDT-backed widgets
#'
#' Owns a `yr::Doc`, its `_attrs` map, and the per-attribute [YAttrStorage]s.
#' A single `_attrs` observer dispatches remote changes to each storage's
#' `remote_changed` signal. Use [make_widget()] to generate subclasses.
#'
#' @export
YAttrWidget <- R6::R6Class(
  "YAttrWidget",
  inherit = WidgetBase,
  public = list(
    #' @description Read the model name registered in the ydoc `_model_name` text.
    #' @return The class name string stored in the ydoc.
    model_name = function() private$get_model_name(),

    #' @description Create a new `YAttrWidget`.
    #' @param ydoc An existing `yr::Doc` to adopt, or `NULL` to create a fresh one.
    initialize = function(ydoc = NULL) {
      super$initialize(ydoc)
      private$.attrs <- self$ydoc$get_or_insert_map("_attrs")
      if (!nzchar(private$get_model_name())) {
        private$set_model_name(class(self)[[1L]])
      }

      private$.attrs$observe(
        function(trans, event) {
          origin <- trans$origin()
          if (is.null(origin) || !origin$equal(REMOTE_ORIGIN)) {
            return()
          }
          keys_info <- event$keys(trans)
          for (k in names(keys_info)) {
            storage <- private$.storages[[k]]
            if (is.null(storage)) {
              next
            }
            new_val <- keys_info[[k]][["inserted"]]
            if (!is.null(new_val)) storage$remote_changed$emit(new_val)
          }
        },
        key = 1L
      )
    },

    #' @description Build and register a [YAttrStorage] for an attribute key.
    #'   `default` is written under `name` only if the key is currently absent
    #'   (so joining from an existing doc preserves remote state). Remote
    #'   changes to this key are dispatched to the storage's `remote_changed`
    #'   signal.
    #' @param name Attribute key.
    #' @param default Default value (Prelim or any R value).
    #' @return The newly created [YAttrStorage].
    register_storage = function(name, default) {
      storage <- YAttrStorage$new(self$ydoc, private$.attrs, name, default)
      private$.storages[[name]] <- storage
      storage
    }
  ),

  private = list(
    .attrs = NULL,

    y_model_name = function() {
      self$ydoc$get_or_insert_text("_model_name")
    },

    set_model_name = function(name) {
      model_text <- private$y_model_name()
      self$ydoc$with_transaction(
        function(trans) model_text$push(trans, name),
        mutable = TRUE,
        origin = LOCAL_ORIGIN
      )
    },

    get_model_name = function() {
      model_text <- private$y_model_name()
      self$ydoc$with_transaction(function(trans) model_text$get_string(trans))
    }
  )
)

#' `Reactive`-shaped storage backed by a root CRDT type on a `yr::Doc`.
#'
#' Each attribute is its own top-level Text/Map/Array root. The kind is
#' inferred from a [yr::Prelim] passed at construction (only its `is_*()`
#' type is consulted; the Prelim's content is not written). `read()` returns
#' the ref; `update()` is unsupported — callers mutate the ref directly.
#' Remote (non-`LOCAL_ORIGIN`) changes fire `remote_changed`.
#'
#' @export
YRootStorage <- R6::R6Class(
  "YRootStorage",
  private = list(
    ref = NULL,

    insert_root = function(ydoc, name, prelim) {
      if (prelim$is_text()) {
        ydoc$get_or_insert_text(name)
      } else if (prelim$is_map()) {
        ydoc$get_or_insert_map(name)
      } else if (prelim$is_array()) {
        ydoc$get_or_insert_array(name)
      } else {
        stop("YRootStorage requires a Text/Map/Array Prelim for '", name, "'.")
      }
    }
  ),

  public = list(
    #' @field remote_changed [Signal] fired with the event on remote changes.
    remote_changed = NULL,

    #' @description Materialize the root ref for `name` from `prelim`'s type.
    #' @param ydoc The `yr::Doc`.
    #' @param name Root name on the doc.
    #' @param prelim A `yr::Prelim` whose `is_text/is_map/is_array` selects
    #'   the root kind. Content is ignored.
    initialize = function(ydoc, name, prelim) {
      if (!inherits(prelim, "Prelim")) {
        stop("YRootStorage requires a yr::Prelim for '", name, "'.")
      }
      private$ref <- private$insert_root(ydoc, name, prelim)
      self$remote_changed <- Signal$new()
      sig <- self$remote_changed
      private$ref$observe(
        function(trans, event) {
          origin <- trans$origin()
          if (is.null(origin) || !origin$equal(REMOTE_ORIGIN)) {
            return()
          }
          sig$emit(event)
        },
        key = 1L
      )
    },

    #' @description Return the underlying root ref.
    read = function() private$ref,

    #' @description Unsupported — root storage is read-only at this layer.
    #' @param value Ignored.
    update = function(value) {
      stop(
        "YRootStorage does not support update(); ",
        "mutate the ref returned by read() directly."
      )
    }
  )
)


#' Base class for root-type-backed widgets
#'
#' Owns a `yr::Doc` and one CRDT root per attribute (Text/Map/Array). Each
#' root carries its own observer that emits the attribute's
#' [YRootStorage]`$remote_changed` signal on non-`LOCAL_ORIGIN` events. Use
#' [make_widget()] with `inherit = YRootWidget` to generate subclasses.
#'
#' @export
YRootWidget <- R6::R6Class(
  "YRootWidget",
  inherit = WidgetBase,
  public = list(
    #' @description Build and register a [YRootStorage] for an attribute name.
    #'   The root kind is selected from `prelim`'s `is_text/is_map/is_array`.
    #'   The storage installs its own per-root observer that filters to
    #'   non-`LOCAL_ORIGIN` events and emits on `remote_changed`.
    #' @param name Root name on the doc.
    #' @param prelim A `yr::Prelim` whose kind selects the root type.
    #' @return The newly created [YRootStorage].
    register_storage = function(name, prelim) {
      storage <- YRootStorage$new(self$ydoc, name, prelim)
      private$.storages[[name]] <- storage
      storage
    }
  )
)

# Build a new instance of `cls` mirroring another widget's ydoc state.
# The diff is applied with LOCAL_ORIGIN so observers stay quiet during the
# initial load; existing keys/roots are then preserved by register_storage's
# idempotent write.
.join_widget <- function(cls, widget, version = "v1") {
  empty_sv <- yr::Doc$new()$with_transaction(function(t) t$state_vector())
  encode_diff <- paste0("encode_diff_", version)
  state <- widget$ydoc$with_transaction(
    function(t) t[[encode_diff]](empty_sv)
  )
  apply_update <- paste0("apply_update_", version)
  doc <- yr::Doc$new()
  doc$with_transaction(
    function(t) t[[apply_update]](state),
    mutable = TRUE,
    origin = LOCAL_ORIGIN
  )
  cls$new(ydoc = doc)
}

# Shared factory backing make_widget. The flavor (map-attr vs root-CRDT)
# is selected by the `inherit` parent class, which dispatches what kind of
# storage `register_storage()` materializes. Everything else — active
# bindings, per-instance defaults that override class-level defaults,
# idempotent seeding on a fresh ydoc, and a uniform $join — is uniform.
.make_widget_class <- function(classname, fields, inherit) {
  nms <- names(fields)
  private_list <- setNames(rep(list(NULL), length(nms)), paste0(".", nms))

  active_list <- setNames(
    lapply(nms, function(nm) {
      pnm <- paste0(".", nm)
      fn <- function(value) {}
      body(fn) <- bquote({
        if (missing(value)) {
          private[[.(pnm)]]$get()
        } else {
          private[[.(pnm)]]$set(value)
        }
      })
      fn
    }),
    nms
  )

  init_fn <- function(ydoc = NULL) {
    super$initialize(ydoc)
    for (nm in nms) {
      val <- get(nm, envir = environment())
      private[[paste0(".", nm)]] <- Reactive$new(
        storage = self$register_storage(nm, val)
      )
    }
  }
  formals(init_fn) <- c(fields, list(ydoc = NULL))

  cls <- R6::R6Class(
    classname,
    inherit = inherit,
    private = private_list,
    active = active_list,
    public = list(initialize = init_fn)
  )

  cls$join <- function(widget, version = "v1") {
    .join_widget(cls, widget, version)
  }

  cls
}

#' Generate a Widget subclass with CRDT-backed attributes
#'
#' Each named attribute becomes an active binding backed by the parent
#' class's storage. With `inherit = YAttrWidget` (the default), reads and
#' writes go through the ydoc's `_attrs` map and `connect()` callbacks fire
#' on both local writes and remote-peer updates. With `inherit = YRootWidget`, each
#' attribute is its own top-level Text/Map/Array root; defaults must be
#' [yr::Prelim]s (only their kind is used — content is not written) and
#' `connect()` fires on remote-peer updates only.
#'
#' @param classname Name of the generated R6 class.
#' @param ...       Named default values, one per attribute. For
#'   `inherit = YAttrWidget`, any R value (Prelims are passed through, others
#'   are wrapped as `yr::Prelim$any`). For `inherit = YRootWidget`, a
#'   [yr::Prelim] whose kind selects the root type. Each default may be
#'   overridden per-instance via `$new(<name> = ...)`; the override seeds a
#'   fresh ydoc and is ignored when joining an existing one.
#' @param inherit   Parent R6 class — [YAttrWidget] or [YRootWidget].
#' @return An [R6::R6Class] generator. Generated classes add a static
#'   `$join(widget)` constructor that mirrors another widget's ydoc state,
#'   alongside the standard `$new(...)`.
#'
#' @examples
#' MyWidget <- make_widget("MyWidget", foo = "", bar = 0L)
#' w  <- MyWidget$new(foo = "hello")
#' w2 <- MyWidget$join(w)                  # joins from w's current ydoc state
#' w$foo                                   # reads from ydoc
#' w$foo <- "hi"                           # writes to ydoc
#' w$connect(foo = function(v) cat("foo changed:", v, "\n"))
#'
#' \dontrun{
#' MyDoc <- make_widget(
#'   "MyDoc",
#'   title = yr::Prelim$text(""),
#'   items = yr::Prelim$array(list(), recursive = FALSE),
#'   inherit = YRootWidget
#' )
#' d <- MyDoc$new()
#' d$ydoc$with_transaction(function(t) d$title$push(t, "hi"), mutable = TRUE)
#' d$connect(title = function(event) cat("title changed\n"))
#' }
#'
#' @export
make_widget <- function(classname, ..., inherit = YAttrWidget) {
  .make_widget_class(classname, list(...), inherit)
}
