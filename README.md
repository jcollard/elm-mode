elm-mode
========

Elm mode for emacs

## Features

1. Syntax highlighting
2. Intelligent indentation
3. Integration with [elm-repl](https://github.com/elm-lang/elm-repl)

## Installation

If your Emacs has `package.el` (which is automatically the case for Emacs >= 24), you can install `elm-mode` from the package in [MELPA](http://melpa.milkbox.net/).

Alternatively, install `elm-mode` manually:

######1. Download or clone this repository

######2. Add the following lines to your .emacs file

```
  (add-to-list 'load-path "~/path/to/elm-mode/")
  (require 'elm-mode)
```

######3. Configure your emacs `PATH` environment variable to include the binary directory for both `elm` and `elm-repl`. 

If you installed `elm` and `elm-repl` using `cabal` on a GNU/Linux distribution, adding the following to your .emacs file will probably be sufficient:

```
    (setenv "PATH" (concat (getenv "PATH") ":~/.cabal/bin"))
    (setq exec-path (append exec-path '("~/.cabal/bin")))
```
