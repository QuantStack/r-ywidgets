# r-ywidgets

Reactive widget models whose state is synchronised through Conflict-free Replicated
Data Type (CRDT) via the `yrs` package.

## Requirements

- R >= 4.1
- `yr`, `R6`
- An xeus-r Jupyter kernel (provides `hera`) and a frontend that speaks the
  `ywidget` comm protocol, e.g. JupyterLab with `yjs-widgets`.

## Usage

`make_comm_widget()` defines an R6 class whose named attributes are backed by a
shared ydoc. Constructing an instance opens a Jupyter comm and keeps the
attributes in sync with the connected frontend.

```r
library(ywidgets)

# Define a widget class with two synced attributes.
Counter <- make_comm_widget("Counter", label = "", count = 0L)

# Constructing opens the comm and starts syncing. Run this in an xeus-r kernel.
w <- Counter$new(label = "clicks")

# Attributes are active bindings: read and write them directly.
w$count
w$count <- w$count + 1L

# React to changes coming from the frontend.
w$connect(count = function(value) {
  message("count is now ", value)
})

```

Writes to an attribute update the ydoc and propagate to the frontend; remote
changes update the attribute and fire the callbacks registered with
`$connect()`.

## See also

- [yr](https://github.com/y-crdt/yr): the R bindings to the Rust
  [yrs](https://github.com/y-crdt/y-crdt) CRDT library.
- [yjs-widgets](github.com/QuantStack/yjs-widgets/) A core CRDT library to build widget
  frontends.
- [ypywidgets](https://github.com/QuantStack/ypywidgets) A similar Python core widget library
  from which this project is inspired.
