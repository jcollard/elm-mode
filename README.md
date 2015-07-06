# elm-mode

Elm mode for EMACS.

## Features

1. Syntax highlighting.
1. Intelligent indentation.
1. Integration with [elm-make](https://github.com/elm-lang/elm-make).
1. Integration with [elm-repl](https://github.com/elm-lang/elm-repl).
1. Integration with [elm-reactor](https://github.com/elm-lang/elm-reactor).

## Indentation

`elm-mode` indentation is based on cycling: every time you insert a new
line there will be one or more indentation levels available for you to
choose from. The exact number is printed in the minibuffer either as
`Sole indentation` or `Indent cycle (n)...` where `n` is the number of
available indentations to choose from. If the automatic indentation
level was not the one you expected simply hit `TAB` to cycle through the
list (note that hitting any other key will cancel the cycle).

## Installation

### MELPA

If your Emacs has `package.el` (which is automatically the case
for Emacs >= 24), you can install `elm-mode` from the package in
[MELPA](http://melpa.milkbox.net/).

### From source

Add this repo to your load-path and `(require 'elm-mode)`. Ensure that
the following dependencies are available:

* [f](https://github.com/rejeep/f.el)
* [let-alist](https://github.com/Malabarba/let-alist)
* [s](https://github.com/magnars/s.el)

This package assumes you are runing Emacs 24 or later.

## Bindings

The following bindings are available in `elm-mode`:

`elm-make` bindings:

<dl>
  <dt><code>C-c C-c</code></dt>
  <dd>Compile the current buffer.</dd>

  <dt><code>C-c M-c</code></dt>
  <dd>Compile the <code>Main.elm</code> file.</dd>
</dl>

`elm-repl` bindings:

<dl>
  <dt><code>C-c C-l</code></dt>
  <dd>Load the current file in a REPL.</dd>

  <dt><code>C-c C-p</code></dt>
  <dd>Push the current region to a REPL.</dd>

  <dt><code>C-c C-e</code></dt>
  <dd>
    Push the current declaration to a REPL (requires <code>haskell-mode</code>
    to be available -- highly experimental).
  </dd>
</dl>

`elm-reactor` bindings:

<dl>
  <dt><code>C-c C-n</code></dt>
  <dd>Preview the current buffer in a browser.</dd>

  <dt><code>C-c C-m</code></dt>
  <dd>Preview the <code>Main.elm</code> file in a browser.</dd>

  <dt><code>C-u C-c C-n</code></dt>
  <dd>Preview the current buffer in a browser in debug mode.</dd>

  <dt><code>C-u C-c C-m</code></dt>
  <dd>Preview the <code>Main.elm</code> file in a browser in debug mode.</dd>
</dl>
