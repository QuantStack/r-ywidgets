#' Pure event emitter
#'
#' Holds a list of subscriber callbacks and broadcasts a value to all of them
#' on [emit()]. Holds no state of its own.
#'
#' @export
Signal <- R6::R6Class(
  "Signal",
  public = list(
    subscribers = NULL,

    #' @description Create a new Signal with no subscribers.
    initialize = function() {
      self$subscribers <- list()
    },

    #' @description Register a callback to be called on each [emit()].
    #' @param f A function accepting a single value.
    connect = function(f) {
      self$subscribers[[length(self$subscribers) + 1]] <- f
    },

    #' @description Broadcast arguments to all subscribers.
    #' @param ... Arguments passed to each subscriber callback.
    emit = function(...) {
      for (f in self$subscribers) {
        f(...)
      }
    }
  )
)

#' Naive local storage backend
#'
#' Stores a value in a private R6 field. This is the default backend for
#' [Reactive].
#'
#' @export
LocalStorage <- R6::R6Class(
  "LocalStorage",
  private = list(value = NULL),
  public = list(
    #' @description Create a new LocalStorage.
    #' @param value Initial value (default `NULL`).
    initialize = function(value = NULL) {
      private$value <- value
    },

    #' @description Return the stored value.
    read = function() {
      private$value
    },

    #' @description Update the stored value if it differs from the current one.
    #' @param value The candidate new value.
    #' @return `TRUE` if the value was changed, `FALSE` otherwise.
    update = function(value) {
      if (identical(private$value, value)) {
        return(FALSE)
      }
      private$value <- value
      TRUE
    }
  )
)

#' Stateful reactive value
#'
#' Wraps a storage backend and owns a [Signal]. Calling [set()] updates the
#' stored value and emits to all subscribers, but only when the new value
#' differs from the current one (checked with [identical()]). Use `$signal`
#' to connect callbacks.
#'
#' @export
Reactive <- R6::R6Class(
  "Reactive",
  private = list(
    storage = NULL
  ),

  public = list(
    #' @field signal The [Signal] emitted when the value changes.
    signal = NULL,

    #' @description Create a new Reactive.
    #' @param value Initial value (default `NULL`).
    #' @param storage A storage backend with `read()` and `write(value)` methods.
    #'   Defaults to a [LocalStorage] initialised with `value`.
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

#' Create a reactive R6 class with named fields
#'
#' Returns a new R6 class whose fields are backed by [Reactive] objects. Each
#' field `f` gets an active binding for read/write access and a `connect_f`
#' public method for subscribing to its signal. The class can be further
#' extended via R6 `inherit`.
#'
#' @param classname Name of the generated R6 class (a string).
#' @param ... Named default values for each reactive field.
#' @return An R6 class generator.
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
