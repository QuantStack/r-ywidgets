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

#' `Reactive` storage backed by a Y.js `attrs` map entry.
#'
#' Implements the same protocol as [RemoteLocalStorage] (`read()`, `update()`,
#' and a public `remote_changed` [Signal]). Writes via `update()` use
#' `LOCAL_ORIGIN`; the owning [Widget] runs the `_attrs` observer and emits on
#' `remote_changed` when a peer changes this key.
#'
#' @export
YdocStorage <- R6::R6Class(
  "YdocStorage",
  private = list(
    ydoc = NULL,
    attrs = NULL,
    key = NULL
  ),
  public = list(
    #' @field remote_changed [Signal] fired on remote changes to the value.
    remote_changed = NULL,

    #' @description Bind the backend to one `attrs` key.
    #' @param ydoc The `yr::Doc`.
    #' @param attrs Its attribute map.
    #' @param key Attribute key to read/write.
    initialize = function(ydoc, attrs, key) {
      private$ydoc <- ydoc
      private$attrs <- attrs
      private$key <- key
      self$remote_changed <- Signal$new()
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
          # TODO should not insert anything unconditionally but allow for setting recursive
          # struct such as Text, Array...
          private$attrs$insert_any(trans, private$key, value)
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
#' Owns a `yr::Doc`, its `_attrs` map, and the per-attribute [YdocStorage]s.
#' A single `_attrs` observer dispatches remote changes to each storage's
#' `remote_changed` signal. Use [make_widget()] to generate subclasses.
#'
#' @export
Widget <- R6::R6Class(
  "Widget",
  public = list(
    #' @field ydoc The underlying `yr::Doc`.
    ydoc = NULL,

    #' @description Read the model name registered in the ydoc `_model_name` text.
    #' @return The class name string stored in the ydoc.
    model_name = function() private$get_model_name(),

    #' @description Create a new `Widget`.
    #' @param ydoc An existing `yr::Doc` to adopt, or `NULL` to create a fresh one.
    initialize = function(ydoc = NULL) {
      self$ydoc <- if (is.null(ydoc)) yr::Doc$new() else ydoc
      private$.attrs <- self$ydoc$get_or_insert_map("_attrs")
      private$.storages <- list()
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

    #' @description Build and register a [YdocStorage] for an attribute key.
    #'   Remote changes to this key will be dispatched to the storage's
    #'   `remote_changed` signal.
    #' @param name Attribute key.
    #' @return The newly created [YdocStorage].
    register_storage = function(name) {
      storage <- YdocStorage$new(self$ydoc, private$.attrs, name)
      private$.storages[[name]] <- storage
      storage
    }
  ),

  private = list(
    .attrs = NULL,
    .storages = NULL,

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

#' Generate a Widget subclass with CRDT-backed attributes
#'
#' Each named attribute becomes an active binding whose reads and writes go
#' through the widget's ydoc `_attrs` map. Subscribers registered via
#' `connect()` fire on both local writes and remote-peer updates.
#'
#' @param classname Name of the generated R6 class.
#' @param ...       Named default values, one per attribute.
#' @param inherit   Parent R6 class.
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
#' @export
make_widget <- function(classname, ..., inherit = Widget) {
  fields <- list(...)
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

  init_fn <- function(ydoc = NULL, .skip_defaults = FALSE) {
    super$initialize(ydoc)
    invisible(lapply(nms, function(nm) {
      val <- get(nm, envir = parent.env(environment()))
      pnm <- paste0(".", nm)
      private[[pnm]] <- Reactive$new(storage = self$register_storage(nm))
      if (!.skip_defaults) private[[pnm]]$set(val)
    }))
  }
  formals(init_fn) <- c(fields, list(ydoc = NULL, .skip_defaults = FALSE))

  # connect(name = fn, ...): subscribe to local and remote changes by field name.
  connect_fn <- function(...) {
    args <- list(...)
    for (nm in names(args)) {
      r <- private[[paste0(".", nm)]]
      r$local_changed$connect(args[[nm]])
      r$remote_changed$connect(args[[nm]])
    }
    invisible(self)
  }

  cls <- R6::R6Class(
    classname,
    inherit = inherit,
    private = private_list,
    active = active_list,
    public = list(initialize = init_fn, connect = connect_fn)
  )

  # join(widget, version = "v1"|"v2"): build an instance mirroring another
  # widget's ydoc state. The diff is applied with LOCAL_ORIGIN so the observer
  # stays quiet during the initial load.
  cls$join <- function(widget, version = "v1") {
    empty_sv <- yr::Doc$new()$with_transaction(function(t) t$state_vector())
    encode_diff <- paste0("encode_diff_", version)
    state <- widget$ydoc$with_transaction(
      function(t) t[[encode_diff]](empty_sv)
    )
    apply_update <- paste0("apply_update_", version)
    doc <- yr::Doc$new()
    # Apply before constructing the instance so that the model name (and field
    # defaults) are already present and the instance does not re-write them
    # under its own client id, which would create ops the source peer lacks.
    doc$with_transaction(
      function(t) t[[apply_update]](state),
      mutable = TRUE,
      origin = LOCAL_ORIGIN
    )
    cls$new(ydoc = doc, .skip_defaults = TRUE)
  }

  cls
}
