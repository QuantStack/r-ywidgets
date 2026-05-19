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

#' Reactive value
#'
#' Wraps a storage backend and a [Signal]. `set()` writes the value and emits
#' on `$signal`, but only when it differs from the current value.
#'
#' @export
Reactive <- R6::R6Class(
  "Reactive",
  private = list(
    storage = NULL
  ),

  public = list(
    #' @field signal [Signal] fired when the value changes.
    signal = NULL,

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
      self$signal <- Signal$new()
    },

    #' @description Return the current value without triggering any signal.
    get = function() {
      private$storage$read()
    },

    #' @description Set a new value. Emits via `$signal` only if the storage
    #'   reports that the value changed.
    #' @param value The new value.
    set = function(value) {
      if (private$storage$update(value)) {
        self$signal$emit(value)
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
      private[[paste0(".", nm)]]$signal$connect(args[[nm]])
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
