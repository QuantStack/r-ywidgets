# --------------------------------------------------------------------------
# Tests
# --------------------------------------------------------------------------

test_that("Widget get and set properties", {
  W <- make_widget("W", foo = "hello", bar = 0L)
  w <- W$new()
  expect_equal(w$foo, "hello")
  expect_equal(w$bar, 0L)

  w <- W$new(foo = "hi", bar = 42L)
  expect_equal(w$foo, "hi")
  expect_equal(w$bar, 42L)

  w$bar <- 99L
  expect_equal(w$bar, 99L)
})

test_that("Widget callback fires when field is updated locally", {
  W <- make_widget("W", x = 0L, y = "")
  w <- W$new()
  fired_x <- NULL
  fired_y <- NULL
  w$connect(x = function(v) fired_x <<- v)
  w$connect(y = function(v) fired_y <<- v)

  w$x <- 7L
  expect_equal(fired_x, 7L)
  expect_null(fired_y)

  w$y <- "changed"
  expect_equal(fired_y, "changed")
})

test_that("Widget callback does not fire when value is unchanged", {
  W <- make_widget("W", x = 0L)
  w <- W$new()
  count <- 0L
  w$connect(x = function(v) count <<- count + 1L)
  w$x <- 0L
  expect_equal(count, 0L)
})

test_that("Widget stores Prelim attribute values as CRDT types", {
  W <- make_widget("W", body = yr::Prelim$text("hi"))
  w <- W$new()
  expect_true(inherits(w$body, "TextRef"))
  expect_equal(
    w$ydoc$with_transaction(function(t) w$body$get_string(t)),
    "hi"
  )

  w$body <- yr::Prelim$text("bye")
  expect_true(inherits(w$body, "TextRef"))
  expect_equal(
    w$ydoc$with_transaction(function(t) w$body$get_string(t)),
    "bye"
  )
})

test_that("Widget join creates widget with field values from source", {
  W <- make_widget("W", foo = "hello", bar = 42L)
  local <- W$new()
  remote <- W$join(local)
  expect_equal(remote$foo, "hello")
  expect_equal(remote$bar, 42L)
})

# A one-way pub/sub byte channel. Ydoc-agnostic.
# `send(bytes)` enqueues a message; `subscribe(cb)` registers a receiver;
# `flush()` drains the queue, invoking every subscriber once per message.
make_wire <- function() {
  queue <- list()
  subscribers <- list()
  list(
    send = function(bytes) queue[[length(queue) + 1L]] <<- bytes,
    subscribe = function(cb) subscribers[[length(subscribers) + 1L]] <<- cb,
    pending = function() length(queue),
    flush = function() {
      msgs <- queue
      queue <<- list()
      for (m in msgs) {
        for (cb in subscribers) {
          cb(m)
        }
      }
    }
  )
}

# Read the current string content from a Prelim-text-backed attribute. Works
# uniformly across flavors because both YAttrWidget (with Prelim$text default)
# and YRootWidget expose the attribute as a TextRef.
read_text <- function(w, name) {
  ref <- w[[name]]
  w$ydoc$with_transaction(function(t) ref$get_string(t))
}

# Per-flavor write: YAttrWidget supports both assign-style (replacing the
# attrs-map slot with a fresh Prelim text) and in-place ref mutation;
# YRootWidget's active binding is read-only, so callers must mutate the ref.
mutate_in_place <- function(w, name, value) {
  ref <- w[[name]]
  w$ydoc$with_transaction(
    function(t) {
      len <- ref$len(t)
      if (len > 0L) {
        ref$remove_range(t, 0L, len)
      }
      ref$push(t, value)
    },
    mutable = TRUE,
    origin = LOCAL_ORIGIN
  )
}

flavors <- list(
  list(
    name = "YAttrWidget (assign)",
    inherit = YAttrWidget,
    write = function(w, name, value) {
      # Full replace via setter
      w[[name]] <- yr::Prelim$text(value)
    }
  ),
  list(
    name = "YAttrWidget (in-place)",
    inherit = YAttrWidget,
    write = mutate_in_place
  ),
  list(
    name = "YRootWidget",
    inherit = YRootWidget,
    write = mutate_in_place
  )
)

for (f in flavors) {
  test_that(
    paste0("manual wire delivers updates between widgets (", f$name, ")"),
    {
      W <- make_widget(
        "W",
        foo = yr::Prelim$text(""),
        bar = yr::Prelim$text(""),
        inherit = f$inherit
      )
      local <- W$new()
      remote <- W$join(local)

      l_to_r <- make_wire()
      r_to_l <- make_wire()

      # Producers: a widget's local-origin changes are pushed as v1 update bytes.
      encode_into <- function(wire) {
        function(trans, event) {
          origin <- trans$origin()
          if (!is.null(origin) && origin$equal(REMOTE_ORIGIN)) {
            return()
          }
          diff <- trans$encode_diff_v1(event$before_state())
          if (length(diff) > 0L) wire$send(diff)
        }
      }
      local$ydoc$observe_transaction_cleanup(encode_into(l_to_r), key = 1L)
      remote$ydoc$observe_transaction_cleanup(encode_into(r_to_l), key = 1L)

      # Consumers: incoming bytes are applied to the peer with REMOTE_ORIGIN.
      apply_into <- function(widget) {
        function(diff) {
          widget$ydoc$with_transaction(
            function(t) t$apply_update_v1(diff),
            mutable = TRUE,
            origin = REMOTE_ORIGIN
          )
        }
      }
      l_to_r$subscribe(apply_into(remote))
      r_to_l$subscribe(apply_into(local))

      f$write(local, "foo", "synced")
      expect_equal(read_text(remote, "foo"), "") # not delivered yet
      expect_equal(l_to_r$pending(), 1L)

      l_to_r$flush()
      expect_equal(read_text(remote, "foo"), "synced")
      expect_equal(l_to_r$pending(), 0L)
      expect_equal(r_to_l$pending(), 0L) # REMOTE_ORIGIN apply does not echo back

      f$write(remote, "bar", "also-synced")
      expect_equal(read_text(local, "bar"), "")
      r_to_l$flush()
      expect_equal(read_text(local, "bar"), "also-synced")
    }
  )
}
