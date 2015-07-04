# elm-mode

Elm mode for EMACS.

## Features

1. Syntax highlighting.
1. Intelligent indentation.
1. Integration with [elm-repl](https://github.com/elm-lang/elm-repl).

## Installation

### From source

Add this repo to your load-path and `(require elm-mode)`. Ensure that
[f](https://github.com/rejeep/f.el) is loaded prior to doing this.

## Bindings

The following bindings are available in `elm-mode`:

<dl>
  <dt><code>C-c C-l</code></dt>
  <dd>Load the current file in a REPL.</dd>

  <dt><code>C-c C-p</code></dt>
  <dd>Push the current region to a REPL.</dd>

  <dt><code>C-c C-e</code></dt>
  <dd>Push the current declaration to a REPL (requires <code>haskell-mode</code> to be available -- highly experimental).</dd>

  <dt><code>C-c C-c</code></dt>
  <dd>Compile the current buffer.</dd>

  <dt><code>C-c C-n</code></dt>
  <dd>Preview the current buffer in a browser.</dd>
</dl>
