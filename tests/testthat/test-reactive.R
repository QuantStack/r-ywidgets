# --------------------------------------------------------------------------
# Signal
# --------------------------------------------------------------------------

test_that("Signal calls all subscribers with the emitted value", {
  sig <- Signal$new()
  received <- c()
  sig$connect(function(v) received <<- c(received, v))
  sig$connect(function(v) received <<- c(received, v * 2))
  sig$emit(3)
  expect_equal(received, c(3, 6))
})

test_that("Signal with no subscribers emits without error", {
  sig <- Signal$new()
  expect_no_error(sig$emit(42))
})

# --------------------------------------------------------------------------
# Reactive
# --------------------------------------------------------------------------

test_that("Reactive get returns the initial value", {
  r <- Reactive$new(10)
  expect_equal(r$get(), 10)
})

test_that("Reactive set updates the value", {
  r <- Reactive$new(1)
  r$set(2)
  expect_equal(r$get(), 2)
})

test_that("Reactive emits signal when value changes", {
  r <- Reactive$new(1)
  fired <- NULL
  r$signal$connect(function(v) fired <<- v)
  r$set(2)
  expect_equal(fired, 2)
})

test_that("Reactive does not emit when value is identical", {
  r <- Reactive$new(1)
  count <- 0L
  r$signal$connect(function(v) count <<- count + 1L)
  r$set(1)
  expect_equal(count, 0L)
})

test_that("Reactive delegates reads and updates to a custom storage backend", {
  log <- c()
  mock_storage <- list(
    value = 0,
    read = function() {
      log <<- c(log, "read")
      mock_storage$value
    },
    update = function(v) {
      log <<- c(log, paste0("update:", v))
      if (identical(mock_storage$value, v)) {
        return(FALSE)
      }
      mock_storage$value <<- v
      TRUE
    }
  )
  r <- Reactive$new(storage = mock_storage)

  r$get() # one read
  r$set(42) # one update (changed)
  r$set(42) # one update (no-op)
  r$get() # one read

  expect_equal(log, c("read", "update:42", "update:42", "read"))
  expect_equal(mock_storage$value, 42)
})

# --------------------------------------------------------------------------
# make_reactive
# --------------------------------------------------------------------------

test_that("make_reactive creates class with correct default values", {
  M <- make_reactive("M", x = 22, y = "hello")
  m <- M$new()
  expect_equal(m$x, 22)
  expect_equal(m$y, "hello")
})

test_that("make_reactive class accepts overridden initial values", {
  M <- make_reactive("M", x = 0, y = 0)
  m <- M$new(x = 5, y = 10)
  expect_equal(m$x, 5)
  expect_equal(m$y, 10)
})

test_that("make_reactive active bindings read and write through Reactive", {
  M <- make_reactive("M", x = 0, y = 0)
  m <- M$new()
  m$x <- 5
  m$y <- 10
  expect_equal(m$x, 5)
  expect_equal(m$y, 10)
})

test_that("make_reactive connect subscribes to named fields", {
  M <- make_reactive("M", x = 0, y = 0)
  m <- M$new()
  fired_x <- NULL
  fired_y <- NULL
  m$connect(x = function(v) fired_x <<- v)
  m$connect(y = function(v) fired_y <<- v)
  m$x <- 7
  expect_equal(fired_x, 7)
  expect_null(fired_y)
  m$y <- 3
  expect_equal(fired_y, 3)
})

test_that("make_reactive connect can subscribe to multiple fields at once", {
  M <- make_reactive("M", x = 0, y = 0)
  m <- M$new()
  log <- c()
  m$connect(
    x = function(v) log <<- c(log, paste0("x=", v)),
    y = function(v) log <<- c(log, paste0("y=", v))
  )
  m$x <- 1
  m$y <- 2
  expect_equal(log, c("x=1", "y=2"))
})
