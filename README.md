# elm-mode

Elm mode for Emacs.

## Features

1. Syntax highlighting.
1. Intelligent indentation.
1. Integration with [elm-make](https://github.com/elm-lang/elm-make).
1. Integration with [elm-repl](https://github.com/elm-lang/elm-repl).
1. Integration with [elm-reactor](https://github.com/elm-lang/elm-reactor).
1. Integration with [elm-package](https://github.com/elm-lang/elm-package).
1. Integration with [elm-oracle][elm-oracle]
1. Integration with [elm-format][elm-format]

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
* [let-alist](https://elpa.gnu.org/packages/let-alist.html)
* [s](https://github.com/magnars/s.el)

This package assumes you are runing Emacs 24 or later.

## Bindings

The following bindings are available in `elm-mode`:

#### `elm-make`

| Keybinding             | Description                                                         |
| ---------------------- | ------------------------------------------------------------------- |
| <kbd>C-c C-c</kbd>     | Compile the current buffer.                                         |
| <kbd>C-u C-c C-c</kbd> | Compile the current buffer, specifying the output file.             |
| <kbd>C-c M-c</kbd>     | Compile the <code>Main.elm</code> file.                             |
| <kbd>C-u C-c M-c</kbd> | Compile the <code>Main.elm</code> file, specifying the output file. |


#### `elm-repl`

| Keybinding         | Description                                                            |
| ------------------ | ---------------------------------------------------------------------- |
| <kbd>C-c C-l</kbd> | Load the current file in a REPL.                                       |
| <kbd>C-c C-p</kbd> | Push the current region to a REPL.                                     |
| <kbd>C-c C-e</kbd> | Push the current decl. to a REPL (requires <code>haskell-mode</code>). |

#### `elm-reactor`

| Keybinding             | Description                                                        |
| ---------------------- | ------------------------------------------------------------------ |
| <kbd>C-c C-n</kbd>     | Preview the current buffer in a browser.                           |
| <kbd>C-u C-c C-n</kbd> | Preview the current buffer in a browser in debug mode.             |
| <kbd>C-c C-m</kbd>     | Preview the <code>Main.elm</code> file in a browser.               |
| <kbd>C-u C-c C-m</kbd> | Preview the <code>Main.elm</code> file in a browser in debug mode. |

#### `elm-package`

| Keybinding             | Description                                                  |
| ---------------------- | ------------------------------------------------------------ |
| <kbd>C-c C-d</kbd>     | View a function's documentation in a browser.                |
| <kbd>C-c C-i</kbd>     | Import a module from one of the dependencies.                |
| <kbd>C-c M-k</kbd>     | Open the package catalog.                                    |
| <kbd>C-u C-c M-k</kbd> | Open the package catalog, refreshing the package list first. |

The following bindings are available in the package list buffer:

| Keybinding   | Description                    |
| ------------ | ------------------------------ |
| <kbd>g</kbd> | Refresh package list.          |
| <kbd>n</kbd> | Next package.                  |
| <kbd>p</kbd> | Previous package.              |
| <kbd>v</kbd> | View package in browser.       |
| <kbd>m</kbd> | Mark package for installation. |
| <kbd>i</kbd> | Mark package for installation. |
| <kbd>u</kbd> | Unmark package.                |
| <kbd>x</kbd> | Install marked packages.       |

#### `elm-format`

The following functionality requires [elm-format][elm-format] to be
installed. `elm-format` does not come with the Elm installer so you
will have to install it manually.

| Keybinding         | Description                              |
| ------------------ | ---------------------------------------- |
| <kbd>C-c C-f</kbd> | Automatically format the current buffer. |

Set or customize `elm-format-on-save` to `t` to apply `elm-format` on
the current buffer on every save.

#### `elm-oracle`

The following functionality requires [elm-oracle][elm-oracle] to be
installed. `elm-oracle` does not come with the Elm installer so you
will have to install it manually.

| Keybinding          | Description                                               |
| ------------------- | --------------------------------------------------------- |
| <kbd>C-c C-t</kbd>  | Show the type of the function at point in the minibuffer. |

`elm-mode` supports auto completion through `elm-oracle`. To
enable basic completion (either with `company-mode` or Emacs'
`completion-at-point`) add `elm-oracle-setup-completion` to the
`elm-mode-hook` like so:

```elisp
(add-hook 'elm-mode-hook #'elm-oracle-setup-completion)
```

If you want to use `company` for autocompletion all you need to do
is add the provided backend to the provider list:

```elisp
(add-to-list 'company-backends 'company-elm)
```

The `company` backend supports `company-quickhelp` as well to display
documentation snippets for the autocompletion candidates.

If you prefer to use `auto-complete` as your completion backend you
can instead do the following after setting up `auto-complete`
according to its manual:

```elisp
(add-hook 'elm-mode-hook #'elm-oracle-setup-ac)
```

Note that the completion process is synchronous so you should set
`ac-auto-start` to a large value or to `nil` otherwise you will
experience slowdown. This will be improved in the future.

Here's a screenshot of `auto-complete` in action:

![auto-complete](/screenshots/auto-complete.png)

[elm-format]: https://github.com/avh4/elm-format#installation-
[elm-oracle]: https://github.com/ElmCast/elm-oracle#installation
