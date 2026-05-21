# Requires hera (xeus-r Comm/CommManager)

# Y.js sync protocol over a Jupyter Comm channel.
#
# Handshake (mirrors ypywidgets CommProvider):
#   1. On open  — kernel sends SyncStep1 (our state vector)
#   2. On Step1 — kernel sends SyncStep2 (diff for the peer)
#   3. On Step2 — kernel applies the peer's state; starts forwarding local changes
#   4. On Update — kernel applies incremental update from peer
#   Local changes → kernel encodes diff since last state → sends Update
#
# Package-namespace bindings are locked after load, so a plain
# `.ywidget_target_registered <<- TRUE` would error. Store the flag inside
# an environment, whose contents stay mutable.
.ywidget_state <- new.env(parent = emptyenv())
.ywidget_state$registered_targets <- character()

.ensure_ywidget_target_registered <- function(target_name) {
  if (target_name %in% .ywidget_state$registered_targets) {
    return(invisible())
  }
  hera::CommManager$register_comm_target(
    target_name,
    function(comm, message) {}
  )
  .ywidget_state$registered_targets <- c(
    .ywidget_state$registered_targets,
    target_name
  )
  invisible()
}

#' Thin transport wrapper around a Jupyter Comm: encodes/decodes `yr::Message`
#' buffers so `CommAttrWidget` never touches the raw hera comm API.
#' @noRd
CommProvider <- R6::R6Class(
  "CommProvider",
  public = list(
    initialize = function(
      target_name,
      description,
      metadata,
      on_remote_message
    ) {
      .ensure_ywidget_target_registered(target_name)
      comm <- hera::CommManager$new_comm(target_name, description = description)
      comm$open(metadata = metadata)
      private$.comm <- comm

      comm$on_message(function(comm_message) {
        # hera's comm message is an R6 object whose name "Message" collides with
        # yr's exported `$.Message` S3 method. As a result `comm_message$buffers`
        # would dispatch into yr and return NULL. `get(..., envir = ...)` does a
        # direct environment lookup and bypasses S3 dispatch.
        # There may be support for R6 in Extendr in the future:
        # https://github.com/extendr/extendr/issues/1051
        buffers <- get("buffers", envir = comm_message, inherits = FALSE)

        # This is a buffer communication protocol on this Comm
        stopifnot(is.list(buffers))
        stopifnot(length(buffers) >= 1L)
        stopifnot(is.raw(buffers[[1]]))

        msg <- yr::Message$decode_v1(buffers[[1]])

        # extendr returns errors as condition values rather than throwing.
        stopifnot(!inherits(msg, "condition"))

        on_remote_message(msg)
      })
    },

    send = function(message) {
      private$.comm$send(buffers = list(message$encode_v1()))
    },

    comm_id = function() private$.comm$id
  ),

  private = list(
    .comm = NULL
  )
)


#' Build a Comm-backed widget class with a chosen ydoc parent
#'
#' R6 fixes the parent class at definition time, so we expose a factory that
#' stamps out a Comm-backed class on top of either [YAttrWidget] or
#' [YRootWidget] (or any other [WidgetBase] subclass). The two common bases
#' are pre-built as [CommAttrWidget] and [CommRootWidget].
#'
#' The generated class opens a Jupyter Comm and runs the Y.js sync protocol
#' over it, mirroring `ypywidgets.CommWidget`: on `SyncStep1` reply with a
#' `SyncStep2` diff; on `SyncStep2` apply the peer's state and (once) start
#' forwarding local changes as `Update`s; on `Update` apply the peer's
#' incremental change.
#'
#' @param inherit Parent R6 class — typically [YAttrWidget] or [YRootWidget].
#' @param classname Name of the generated R6 class.
#' @return An [R6::R6Class] generator.
#' @noRd
.make_comm_widget_class <- function(
  inherit = YAttrWidget,
  classname = "CommAttrWidget"
) {
  R6::R6Class(
    classname,
    inherit = inherit,

    public = list(
      #' @description Open a Comm on the `"ywidget"` target and send `SyncStep1`.
      #' @param ydoc Optional existing `yr::Doc` to adopt.
      #' @param comm_metadata Overrides the default metadata sent on comm open.
      initialize = function(ydoc = NULL, comm_metadata = NULL) {
        super$initialize(ydoc)

        if (is.null(comm_metadata$ymodel_name)) {
          comm_metadata$ymodel_name <- class(self)[[1L]]
        }
        if (is.null(comm_metadata$create_ydoc)) {
          comm_metadata$create_ydoc <- is.null(ydoc)
        }

        # hera invokes the CommProvider callback with R6 bindings broken, so we
        # capture `self` explicitly and re-enter via `widget$on_remote_message`.
        widget <- self
        private$.comm_provider <- CommProvider$new(
          target_name = "ywidget",
          description = comm_metadata$ymodel_name,
          metadata = comm_metadata,
          on_remote_message = function(msg) widget$on_remote_message(msg)
        )

        state_vector <- self$ydoc$with_transaction(function(trans) {
          trans$state_vector()
        })
        private$.comm_provider$send(
          yr::Message$new(yr::SyncMessage$from_sync_step1(state_vector))
        )
      },

      #' @description Dispatch an incoming `yr::Message` to its sync handler.
      #'   Public so hera callbacks can re-enter via a captured `self`, where R6
      #'   bindings are otherwise broken.
      #' @param msg A `yr::Message`.
      on_remote_message = function(msg) {
        if (msg$is_sync_message()) {
          sync_msg <- msg$inner()
          if (isTRUE(sync_msg$is_sync_step1())) {
            private$.on_sync_step1(sync_msg)
          } else if (isTRUE(sync_msg$is_sync_step2())) {
            private$.on_sync_step2(sync_msg)
          } else if (isTRUE(sync_msg$is_update())) {
            private$.on_update(sync_msg)
          }
        } else {
          # Only sync message are handled for now
          return(invisible())
        }
      },

      #' @description The underlying comm id (used to build the display payload).
      comm_id = function() private$.comm_provider$comm_id()
    ),

    private = list(
      .comm_provider = NULL,
      .observer_registered = FALSE,

      .apply_remote = function(update_bytes) {
        self$ydoc$with_transaction(
          function(trans) trans$apply_update_v1(update_bytes),
          mutable = TRUE,
          origin = REMOTE_ORIGIN
        )
      },

      .on_sync_step1 = function(sync_msg) {
        diff <- self$ydoc$with_transaction(function(trans) {
          trans$encode_diff_v1(sync_msg$state_vector())
        })
        msg <- yr::Message$new(yr::SyncMessage$from_sync_step2(diff))
        private$.comm_provider$send(msg)
      },

      .on_sync_step2 = function(sync_msg) {
        private$.apply_remote(sync_msg$data())
        if (private$.observer_registered) {
          return()
        }
        private$.observer_registered <- TRUE
        # Capture provider directly — extendr may break R6 bindings in callbacks.
        provider <- private$.comm_provider
        self$ydoc$observe_transaction_cleanup(
          function(trans, event) {
            origin <- trans$origin()
            if (is.null(origin) || !origin$equal(LOCAL_ORIGIN)) {
              return()
            }
            diff <- trans$encode_diff_v1(event$before_state())
            if (length(diff) == 0L) {
              return()
            }
            msg <- yr::Message$new(yr::SyncMessage$from_update(diff))
            provider$send(msg)
          },
          key = 1L
        )
      },

      .on_update = function(sync_msg) {
        private$.apply_remote(sync_msg$data())
      }
    )
  )
}

#' Widget backed by a Jupyter Comm and a CRDT ydoc, with map-attr storage
#'
#' Opens a Jupyter Comm on the `"ywidget"` target and runs the Y.js sync
#' protocol on top of [YAttrWidget]'s map-attr storage. Constructor:
#' `CommAttrWidget$new(ydoc = NULL, comm_metadata = NULL)`.
#' @export
CommAttrWidget <- R6::R6Class(
  "CommAttrWidget",
  inherit = .make_comm_widget_class(YAttrWidget, ".CommAttrWidgetBase")
)

#' Widget backed by a Jupyter Comm and a CRDT ydoc, with root-CRDT storage
#'
#' Like [CommAttrWidget] but on top of [YRootWidget]'s per-root storage.
#' Constructor: `CommRootWidget$new(ydoc = NULL, comm_metadata = NULL)`.
#' @export
CommRootWidget <- R6::R6Class(
  "CommRootWidget",
  inherit = .make_comm_widget_class(YRootWidget, ".CommRootWidgetBase")
)

#' @exportS3Method hera::mime_types
mime_types.CommAttrWidget <- function(x) {
  c("text/plain", "application/vnd.jupyter.ywidget-view+json")
}

#' @exportS3Method hera::mime_bundle
mime_bundle.CommAttrWidget <- function(x, mimetypes = mime_types(x), ...) {
  list(
    data = list(
      "text/plain" = "",
      "application/vnd.jupyter.ywidget-view+json" = list(
        version_major = 2L,
        version_minor = 0L,
        model_id = x$comm_id()
      )
    ),
    # Serialize as dict, not list
    metadata = structure(list(), names = character(0))
  )
}

#' @exportS3Method hera::mime_types
mime_types.CommRootWidget <- mime_types.CommAttrWidget

#' @exportS3Method hera::mime_bundle
mime_bundle.CommRootWidget <- mime_bundle.CommAttrWidget

#' Generate a Comm-backed widget subclass with named CRDT-backed attributes
#'
#' Like [make_widget()], but the generated class inherits from a Comm-backed
#' base (default [CommAttrWidget]; pass [CommRootWidget] for root storage),
#' so each instance opens a Jupyter Comm and syncs its ydoc over it.
#'
#' @inheritParams make_widget
#' @return An [R6::R6Class] generator producing Comm-backed widget subclasses.
#' @export
make_comm_widget <- function(
  classname,
  ...,
  comm_metadata = NULL,
  inherit = CommAttrWidget
) {
  md <- comm_metadata
  wrapper <- R6::R6Class(
    paste0(classname, "Base"),
    inherit = inherit,
    public = list(
      initialize = function(ydoc = NULL) {
        super$initialize(ydoc = ydoc, comm_metadata = md)
      }
    )
  )
  make_widget(classname, ..., inherit = wrapper)
}
