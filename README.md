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
1. Integration with [elm-test][elm-test]

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
* [dash](https://github.com/magnars/dash.el)

This package assumes you are runing Emacs 24 or later.

## Bindings

The following bindings are available in `elm-mode`:

#### `TAGS`

| Keybinding             | Description                                                         |
| ---------------------- | ------------------------------------------------------------------- |
| <kbd>C-c M-t</kbd>     | Generate a TAGS file for the current project.                       |
| <kbd>M-.</kbd>         | Jump to tag at point.                                               |
| <kbd>M-,</kbd>         | Jump to previous location after visiting a tag.                     |

TAGS file generation requires that the UNIX `find`, `egrep` commands
and the Emacs `etags` command be available in your path.

Set or customize `elm-tags-on-save` to `t` to generate a new TAGS file
for the current project on every save.

Set or customize `elm-tags-exclude-elm-stuff` to `nil` to include
source files inside `elm-stuff` directories when generating the TAGS
file.

#### `elm-make`

| Keybinding             | Description                                                                       |
| ---------------------- | --------------------------------------------------------------------------------- |
| <kbd>C-c C-c</kbd>     | Compile the current buffer.                                                       |
| <kbd>C-u C-c C-c</kbd> | Compile the current buffer, specifying the output file.                           |
| <kbd>C-c M-c</kbd>     | Compile the main elm file.                                           |
| <kbd>C-u C-c M-c</kbd> | Compile the main elm file, specifying the output file.               |
| <kbd>C-c C-a</kbd>     | Add missing type annotations to the current buffer.                               |
| <kbd>C-u C-c C-a</kbd> | Add missing type annotations to the current buffer, prompting before each change. |
| <kbd>C-c C-r</kbd>     | Clean up imports in the current buffer.                                           |
| <kbd>C-u C-c C-r</kbd> | Clean up imports in the current buffer, prompting before each change.             |


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
| <kbd>C-c C-m</kbd>     | Preview the main elm file in a browser.               |
| <kbd>C-u C-c C-m</kbd> | Preview the main elm file in a browser in debug mode. |

#### `elm-package`

| Keybinding             | Description                                                  |
| ---------------------- | ------------------------------------------------------------ |
| <kbd>C-c C-d</kbd>     | View a function's documentation in a browser.                |
| <kbd>C-c C-i</kbd>     | Import a module from one of the dependencies.                |
| <kbd>C-c C-s</kbd>     | Sort the imports in the current file.                        |
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

Set or customize `elm-sort-imports-on-save` to `t` to apply
`elm-sort-imports` on the current buffer on every save.

#### `elm-oracle`

The following functionality requires [elm-oracle][elm-oracle] to be
installed.  `elm-oracle` does not come with the Elm installer so you
will have to install it manually.

| Keybinding          | Description                                               |
| ------------------- | --------------------------------------------------------- |
| <kbd>C-c C-t</kbd>  | Show the type of the function at point in the minibuffer. |
| <kbd>C-c M-d</kbd>  | Show the documentation of the function at point.          |

`elm-mode` supports auto-completion through `elm-oracle`.  To
use [company-mode][company] for auto-completion, add the Elm backend
to the backend list:

```elisp
(add-to-list 'company-backends 'company-elm)
```

The Company backend supports `company-quickhelp` to display
documentation snippets for the autocompletion candidates.

Here's a screenshot of `company-mode` in action:

![company-mode](/screenshots/company-mode.png)

#### `elm-format`

The following functionality requires [elm-format][elm-format] to be
installed. `elm-format` does not come with the Elm installer so you
will have to install it manually.

| Keybinding         | Description                              |
| ------------------ | ---------------------------------------- |
| <kbd>C-c C-f</kbd> | Automatically format the current buffer. |

Set or customize `elm-format-on-save` to `t` to apply `elm-format` on
the current buffer on every save.

Set or customize `elm-format-elm-version` to change which version of
Elm to format against. Valid options are `0.17` and `0.16`. The
default is `0.17`.

#### `elm-test`

The following functionality requires [elm-test][elm-test] to be
installed.  `elm-test` does not come with the Elm installer so you
will have to install it manually.

| Keybinding         | Description                                 |
| ------------------ | ------------------------------------------- |
| <kbd>C-c C-v</kbd>   | Run the test suite for the current project. |


[company-mode]: http://company-mode.github.io/
[elm-format]: https://github.com/avh4/elm-format#installation-
[elm-oracle]: https://github.com/ElmCast/elm-oracle#installation
[elm-test]: https://github.com/rtfeldman/node-test-runner
