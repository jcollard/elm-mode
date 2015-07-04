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
  <dt>`C-c C-l`</dt>
  <dd>Load the current file in a REPL.</dd>

  <dt>`C-c C-p`</dt>
  <dd>Push the current region to a REPL.</dd>

  <dt>`C-c C-e`</dt>
  <dd>Push the current declaration to a REPL (requires `haskell-mode` to be available -- highly experimental).</dd>

  <dt>`C-c C-c`</dt>
  <dd>Compile the current buffer.</dd>

  <dt>`C-c C-n`</dt>
  <dd>Preview the current buffer in a browser.</dd>
</dl>
