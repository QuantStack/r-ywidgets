#' Event emitter.
#'
#' Broadcasts a value to all registered callbacks. Holds no state of its own.
#'
#' @export
Signal <- R6::R6Class(
  "Signal",
  private = list(subscribers = list()),
  public = list(
    #' @description Register a callback invoked on every `emit()`.
    #' @param f A function accepting the emitted arguments.
    connect = function(f) {
      private$subscribers[[length(private$subscribers) + 1]] <- f
    },

    #' @description Invoke every registered callback with `...`.
    #' @param ... Arguments forwarded to each callback.
    emit = function(...) {
      for (f in private$subscribers) {
        f(...)
      }
    }
  )
)

#' Default in-memory storage backend for `Reactive`.
#'
#' @export
LocalStorage <- R6::R6Class(
  "LocalStorage",
  private = list(value = NULL),
  public = list(
    #' @description Create the backend.
    #' @param value Initial value.
    initialize = function(value = NULL) {
      private$value <- value
    },

    #' @description Return the stored value.
    read = function() {
      private$value
    },

    #' @description Store `value`; no-op when unchanged.
    #' @param value New value.
    #' @return `TRUE` iff the value was replaced.
    update = function(value) {
      if (identical(private$value, value)) {
        return(FALSE)
      }
      private$value <- value
      TRUE
    }
  )
)

#' Storage backend with a remote-change [Signal].
#'
#' Extends [LocalStorage] with a public `remote_changed` signal fired when the
#' value changes for reasons outside of `update()`. The in-memory backend has
#' no external source, so `remote_changed` never fires; subclasses backed by
#' shared state emit on it when they observe remote updates.
#'
#' @export
RemoteLocalStorage <- R6::R6Class(
  "RemoteLocalStorage",
  inherit = LocalStorage,
  public = list(
    #' @field remote_changed [Signal] fired on remote changes to the value.
    remote_changed = NULL,

    #' @description Create the backend.
    #' @param value Initial value.
    initialize = function(value = NULL) {
      super$initialize(value)
      self$remote_changed <- Signal$new()
    }
  )
)

#' Reactive value
#'
#' Wraps a storage backend and a [Signal]. `set()` writes the value and emits
#' on `$local_changed`, but only when it differs from the current value. If
#' the storage exposes a `remote_changed` signal, it is re-exposed as
#' `$remote_changed`; otherwise that field is `NULL`.
#'
#' @export
Reactive <- R6::R6Class(
  "Reactive",
  private = list(
    storage = NULL
  ),

  public = list(
    #' @field local_changed [Signal] fired when the value is changed via `set()`.
    local_changed = NULL,

    #' @field remote_changed [Signal] from the storage backend, or `NULL` if
    #'   the backend does not expose one.
    remote_changed = NULL,

    #' @description Create a reactive value.
    #' @param value Initial value.
    #' @param storage Backend implementing `read()` and `update(value)`;
    #'   defaults to in-memory storage.
    initialize = function(value = NULL, storage = NULL) {
      private$storage <- if (is.null(storage)) {
        LocalStorage$new(value)
      } else {
        storage
      }
      self$local_changed <- Signal$new()
      if (!is.null(private$storage$remote_changed)) {
        self$remote_changed <- private$storage$remote_changed
      }
    },

    #' @description Return the current value without triggering any signal.
    get = function() {
      private$storage$read()
    },

    #' @description Set a new value. Emits via `$local_changed` only if the
    #'   storage reports that the value changed.
    #' @param value The new value.
    set = function(value) {
      if (private$storage$update(value)) {
        self$local_changed$emit(value)
      }
    }
  )
)

#' Generate a reactive R6 class
#'
#' Each named argument becomes a [Reactive]-backed field exposed as an active
#' binding, plus a `connect()` method to subscribe callbacks by field name.
#'
#' @param classname Name of the generated R6 class.
#' @param ... Named default values, one per reactive field.
#' @return An [R6::R6Class] generator.
#' @export
make_reactive <- function(classname, ...) {
  fields <- list(...)
  nms <- names(fields)

  private_list <- setNames(rep(list(NULL), length(nms)), paste0(".", nms))

  # Use bquote to inline the private field name as a literal so R6's
  # environment rebinding at instantiation time does not break the lookup.
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

  connect_fn <- function(...) {
    args <- list(...)
    for (nm in names(args)) {
      private[[paste0(".", nm)]]$local_changed$connect(args[[nm]])
    }
    invisible(self)
  }

  init_fn <- function() {}
  formals(init_fn) <- fields
  body(init_fn) <- as.call(c(
    list(quote(`{`)),
    lapply(nms, function(nm) {
      bquote(private[[.(paste0(".", nm))]] <- Reactive$new(.(as.name(nm))))
    })
  ))

  R6::R6Class(
    classname,
    private = private_list,
    active = active_list,
    public = list(initialize = init_fn, connect = connect_fn)
  )
}
