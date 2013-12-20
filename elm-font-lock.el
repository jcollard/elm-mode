;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(require 'font-lock)
(with-no-warnings (require 'cl))

(defcustom elm-font-lock-symbols nil
  "Display \\ and -> and such using symbols in fonts.
This may sound like a neat trick, but be extra careful: it changes the
alignment and can thus lead to nasty surprises w.r.t layout.
If t, try to use whichever font is available.  Otherwise you can
set it to a particular font of your preference among `japanese-jisx0208'
and `unicode'."
  :group 'elm
  :type '(choice (const nil)
                 (const t)
                 (const unicode)
                 (const japanese-jisx0208)))

;; Use new vars for the font-lock faces.  The indirection allows people to
;; use different faces than in other modes, as before.
(defvar elm-keyword-face 'font-lock-keyword-face)
(defvar elm-constructor-face 'font-lock-type-face)
;; This used to be `font-lock-variable-name-face' but it doesn't result in
;; a highlighting that's consistent with other modes (it's mostly used
;; for function defintions).
(defvar elm-definition-face 'font-lock-function-name-face)
;; This is probably just wrong, but it used to use
;; `font-lock-function-name-face' with a result that was not consistent with
;; other major modes, so I just exchanged with `elm-definition-face'.
(defvar elm-operator-face 'font-lock-variable-name-face)
(defvar elm-default-face nil)
(defvar elm-literate-comment-face 'font-lock-doc-face
  "Face with which to fontify literate comments.
Set to `default' to avoid fontification of them.")

(defconst elm-emacs21-features (string-match "[[:alpha:]]" "x")
  "Non-nil if we have regexp char classes.
Assume this means we have other useful features from Emacs 21.")

(defun elm-font-lock-compose-symbol (alist)
  "Compose a sequence of ascii chars into a symbol.
Regexp match data 0 points to the chars."
  ;; Check that the chars should really be composed into a symbol.
  (let* ((start (match-beginning 0))
         (end (match-end 0))
         (syntaxes (cond
                    ((eq (char-syntax (char-after start)) ?w) '(?w))
                    ;; Special case for the . used for qualified names.
                    ((and (eq (char-after start) ?\.) (= end (1+ start)))
                     '(?_ ?\\ ?w))
                    (t '(?_ ?\\))))
         sym-data)
    (if (or (memq (char-syntax (or (char-before start) ?\ )) syntaxes)
            (memq (char-syntax (or (char-after end) ?\ )) syntaxes)
            (memq (get-text-property start 'face)
                  '(font-lock-doc-face font-lock-string-face
                    font-lock-comment-face))
            (and (consp (setq sym-data (cdr (assoc (match-string 0) alist))))
                 (let ((pred (cadr sym-data)))
                   (setq sym-data (car sym-data))
                   (funcall pred start))))
        ;; No composition for you.  Let's actually remove any composition
        ;; we may have added earlier and which is now incorrect.
        (remove-text-properties start end '(composition))
      ;; That's a symbol alright, so add the composition.
      (compose-region start end sym-data)))
  ;; Return nil because we're not adding any face property.
  nil)


;; The font lock regular expressions.
(defun elm-font-lock-keywords-create (literate)
  "Create fontification definitions for Elm scripts.
Returns keywords suitable for `font-lock-keywords'."
  (let* (
         ;; Most names are borrowed from the lexical syntax of the Elm
         ;; report.
         ;; Some of these definitions have been superseded by using the
         ;; syntax table instead.

         ;; (ASCsymbol "-!#$%&*+./<=>?@\\\\^|~")
         ;; Put the minus first to make it work in ranges.

         ;; We allow _ as the first char to fit GHC
         (varid "\\b[[:lower:]_][[:alnum:]'_]*\\b")
         ;; We allow ' preceding conids because of DataKinds/PolyKinds
         (conid "\\b'?[[:upper:]][[:alnum:]'_]*\\b")
         (modid (concat "\\b" conid "\\(\\." conid "\\)*\\b"))
         (qvarid (concat modid "\\." varid))
         (qconid (concat modid "\\." conid))
         (sym
          ;; We used to use the below for non-Emacs21, but I think the
          ;; regexp based on syntax works for other emacsen as well.  -- Stef
          ;; (concat "[" symbol ":]+")
          ;; Add backslash to the symbol-syntax chars.  This seems to
          ;; be thrown for some reason by backslash's escape syntax.
          "\\(\\s_\\|\\\\\\)+")

         ;; Reserved operations
         (reservedsym
          (concat "\\S_"
                  ;; -> . .. :: : <~ =
                  "\\(->\\|\\.\\|\\.\\.\\|::\\|:\\|<~\\|=\\)"
                  "\\S_"))
         ;; Reserved identifiers
         (reservedid
          (concat "\\<"
		  ;; case data else if import open in let module of then type
                  "\\(_\\|case\\|data\\|else\\|if\\|import\\|open\\|in\\|let\\|module\\|of\\|then\\|type\\)"
                  "\\>"))

         ;; This unreadable regexp matches strings and character
         ;; constants.  We need to do this with one regexp to handle
         ;; stuff like '"':"'".  The regexp is the composition of
         ;; "([^"\\]|\\.)*" for strings and '([^\\]|\\.[^']*)' for
         ;; characters, allowing for string continuations.
         ;; Could probably be improved...
         (string-and-char
          (concat "\\(\\(\"\\|" line-prefix "[ \t]*\\\\\\)\\([^\"\\\\\n]\\|\\\\.\\)*\\(\"\\|\\\\[ \t]*$\\)\\|'\\([^'\\\\\n]\\|\\\\.[^'\n]*\\)'\\)"))

         ;; Top-level declarations
         (topdecl-var
          (concat line-prefix "\\(" varid "\\)\\s-*"
                  ;; A toplevel declaration can be followed by a definition
                  ;; (=), a type (:), a guard, or a pattern which can
                  ;; either be a variable, a constructor, a parenthesized
                  ;; thingy, or an integer or a string.
                  "\\(" varid "\\|" conid "\\|:\\|=\\||\\|\\s(\\|[0-9\"']\\)"))
         (topdecl-var2
          (concat line-prefix "\\(" varid "\\|" conid "\\)\\s-*`\\(" varid "\\)`"))
         (topdecl-sym
          (concat line-prefix "\\(" varid "\\|" conid "\\)\\s-*\\(" sym "\\)"))
         (topdecl-sym2 (concat line-prefix "(\\(" sym "\\))"))

	 (comments
          (concat ""))

         keywords)

    (setq keywords
          `(;; NOTICE the ordering below is significant
            ;;
            ("^<<<<<<< .*$" 0 'font-lock-warning-face t)
            ("^=======" 0 'font-lock-warning-face t)
            ("^>>>>>>> .*$" 0 'font-lock-warning-face t)
            ("^#.*$" 0 'font-lock-preprocessor-face t)
            ,@(unless elm-emacs21-features ;Supports nested comments?
                ;; Expensive.
                `((,string-and-char 1 font-lock-string-face)))


            (,reservedid 1 (symbol-value 'elm-keyword-face))
            (,reservedsym 1 (symbol-value 'elm-operator-face))
            ;; Special case for `as', `hiding', `safe' and `qualified', which are
            ;; keywords in import statements but are not otherwise reserved.
            ("\\<import[ \t]+\\(?:\\(safe\\>\\)[ \t]*\\)?\\(?:\\(qualified\\>\\)[ \t]*\\)?[^ \t\n()]+[ \t]*\\(?:\\(\\<as\\>\\)[ \t]*[^ \t\n()]+[ \t]*\\)?\\(\\<hiding\\>\\)?"
             (1 (symbol-value 'elm-keyword-face) nil lax)
             (2 (symbol-value 'elm-keyword-face) nil lax)
             (3 (symbol-value 'elm-keyword-face) nil lax)
             (4 (symbol-value 'elm-keyword-face) nil lax))

            (,reservedsym 1 (symbol-value 'elm-operator-face))
            ;; Special case for `foreign import'
            ;; keywords in foreign import statements but are not otherwise reserved.
            ("\\<\\(foreign\\)[ \t]+\\(import\\)[ \t]+\\(?:\\(ccall\\|stdcall\\|cplusplus\\|jvm\\|dotnet\\)[ \t]+\\)?\\(?:\\(safe\\|unsafe\\|interruptible\\)[ \t]+\\)?"
             (1 (symbol-value 'elm-keyword-face) nil lax)
             (2 (symbol-value 'elm-keyword-face) nil lax)
             (3 (symbol-value 'elm-keyword-face) nil lax)
             (4 (symbol-value 'elm-keyword-face) nil lax))

            (,reservedsym 1 (symbol-value 'elm-operator-face))
            ;; Special case for `foreign export'
            ;; keywords in foreign export statements but are not otherwise reserved.
            ("\\<\\(foreign\\)[ \t]+\\(export\\)[ \t]+\\(?:\\(ccall\\|stdcall\\|cplusplus\\|jvm\\|dotnet\\)[ \t]+\\)?"
             (1 (symbol-value 'elm-keyword-face) nil lax)
             (2 (symbol-value 'elm-keyword-face) nil lax)
             (3 (symbol-value 'elm-keyword-face) nil lax))

            ;; Toplevel Declarations.
            ;; Place them *before* generic id-and-op highlighting.
            (,topdecl-var  (1 (symbol-value 'elm-definition-face)))
            (,topdecl-var2 (2 (symbol-value 'elm-definition-face)))
            (,topdecl-sym  (2 (symbol-value 'elm-definition-face)))
            (,topdecl-sym2 (1 (symbol-value 'elm-definition-face)))

            ;; These four are debatable...
            ("(\\(,*\\|->\\))" 0 (symbol-value 'elm-constructor-face))
            ("\\[\\]" 0 (symbol-value 'elm-constructor-face))
            ;; Expensive.
            (,qvarid 0 (symbol-value 'elm-default-face))
            (,qconid 0 (symbol-value 'elm-constructor-face))
            (,(concat "\`" varid "\`") 0 (symbol-value 'elm-operator-face))
            ;; Expensive.
            (,conid 0 (symbol-value 'elm-constructor-face))

            ;; Very expensive.
            (,sym 0 (if (eq (char-after (match-beginning 0)) ?:)
                        elm-constructor-face
                      elm-operator-face))))
    (unless (boundp 'font-lock-syntactic-keywords)
      (case literate
         (setq keywords
               `(@keywords))))
    keywords))

(defconst elm-basic-syntactic-keywords
  '(;; Character constants (since apostrophe can't have string syntax).
    ;; Beware: do not match something like 's-}' or '\n"+' since the first '
    ;; might be inside a comment or a string.
    ;; This still gets fooled with "'"'"'"'"'"', but ... oh well.
    ("\\Sw\\('\\)\\([^\\'\n]\\|\\\\.[^\\'\n \"}]*\\)\\('\\)" (1 "|") (3 "|"))
    ;; The \ is not escaping in \(x,y) -> x + y.
    ("\\(\\\\\\)(" (1 "."))
    ;; The second \ in a gap does not quote the subsequent char.
    ;; It's probably not worth the trouble, tho.
    ;; ("^[ \t]*\\(\\\\\\)" (1 "."))
    ;; Deal with instances of `--' which don't form a comment
    ("\\s_\\{3,\\}" (0 (cond ((numberp (nth 4 (syntax-ppss)))
                              ;; There are no such instances inside nestable comments
                              nil)
                             ((string-match "\\`-*\\'" (match-string 0))
                              ;; Sequence of hyphens.  Do nothing in
                              ;; case of things like `{---'.
                              nil)
                             (t "_")))) ; other symbol sequence
    ))




(defconst elm-font-lock-keywords
  (elm-font-lock-keywords-create nil)
  "Font lock definitions for non-literate Elm.")

;;;###autoload
(defun elm-font-lock-choose-keywords ()
  (let ((literate (if (boundp 'elm-literate) elm-literate)))
    (case literate
      (t elm-font-lock-keywords))))

(defun elm-font-lock-choose-syntactic-keywords ()
  (let ((literate (if (boundp 'elm-literate) elm-literate)))
    (case literate
      (t elm-basic-syntactic-keywords))))

(defun elm-font-lock-defaults-create ()
  "Locally set `font-lock-defaults' for Elm."
  (set (make-local-variable 'font-lock-defaults)
       '(elm-font-lock-choose-keywords
         nil nil ((?\' . "w") (?_  . "w")) nil
         (font-lock-syntactic-keywords
          . elm-font-lock-choose-syntactic-keywords)
         (font-lock-syntactic-face-function
          . elm-syntactic-face-function)
         ;; Get help from font-lock-syntactic-keywords.
         (parse-sexp-lookup-properties . t))))

;; The main functions.
(defun turn-on-elm-font-lock ()
  "Turns on font locking in current buffer for Elm 1.4 scripts.

Changes the current buffer's `font-lock-defaults', and adds the
following variables:

   `elm-keyword-face'      for reserved keywords and syntax,
   `elm-constructor-face'  for data- and type-constructors, class names,
                               and module names,
   `elm-operator-face'     for symbolic and alphanumeric operators,
   `elm-default-face'      for ordinary code.

The variables are initialised to the following font lock default faces:

   `elm-keyword-face'      `font-lock-keyword-face'
   `elm-constructor-face'  `font-lock-type-face'
   `elm-operator-face'     `font-lock-function-name-face'
   `elm-default-face'      <default face>

Two levels of fontification are defined: level one (the default)
and level two (more colour).  The former does not colour operators.
Use the variable `font-lock-maximum-decoration' to choose
non-default levels of fontification.  For example, adding this to
.emacs:

  (setq font-lock-maximum-decoration '((elm-mode . 2) (t . 0)))

uses level two fontification for `elm-mode' and default level for
all other modes.  See documentation on this variable for further
details.

To alter an attribute of a face, add a hook.  For example, to change
the foreground colour of comments to brown, add the following line to
.emacs:

  (add-hook 'elm-font-lock-hook
      (lambda ()
          (set-face-foreground 'elm-comment-face \"brown\")))

Note that the colours available vary from system to system.  To see
what colours are available on your system, call
`list-colors-display' from emacs.

To turn font locking on for all Elm buffers, add this to .emacs:

  (add-hook 'elm-mode-hook 'turn-on-elm-font-lock)

To turn font locking on for the current buffer, call
`turn-on-elm-font-lock'.  To turn font locking off in the current
buffer, call `turn-off-elm-font-lock'.

Invokes `elm-font-lock-hook' if not nil."
  (elm-font-lock-defaults-create)
  (run-hooks 'elm-font-lock-hook)
  (turn-on-font-lock))

(defun turn-off-elm-font-lock ()
  "Turns off font locking in current buffer."
  (font-lock-mode -1))

;; Provide ourselves:

(provide 'elm-font-lock)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; tab-width: 8
;; End:

;;; elm-font-lock.el ends here
