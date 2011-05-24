;;; filladapt-pat.el --- add or remove some filladapt patterns

;; Copyright 2007, 2008, 2009, 2010, 2011 Kevin Ryde

;; Author: Kevin Ryde <user42@zip.com.au>
;; Version: 3
;; Keywords: convenience, filladapt
;; URL: http://user42.tuxfamily.org/filladapt-pat/index.html
;; EmacsWiki: FillAdapt

;; filladapt-pat.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by the
;; Free Software Foundation; either version 3, or (at your option) any later
;; version.
;;
;; filladapt-pat.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General
;; Public License for more details.
;;
;; You can get a copy of the GNU General Public License online at
;; <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is some functions to add or remove patterns filladapt uses for
;; paragraph filling.  They're designed to go in a mode hook and operate
;; buffer-local, but can also be used interactively for occasional changes
;; to the patterns.
;;
;; The setups work whether or not filladapt has loaded yet and do nothing if
;; you haven't turned on filladapt, or for that matter if you don't have it
;; at all.

;;; Install:

;; Put filladapt-pat.el in one of your `load-path' directories, and in your
;; .emacs for example
;;
;;     (require 'filladapt-pat)
;;     (add-hook 'html-mode-hook 'filladapt-pat-bullet-<li>)
;;
;; There's autoload cookies for the functions, if you know how to use
;; `update-file-autoloads' and friends.  But there's so many small things
;; that autoloads may come out just as big as the actual code!

;;; History:
;;
;; Version 1 - the first version
;; Version 2 - new filladapt-pat-version-bullet misc cleanups
;; Version 3 - new filladapt-pat-no-citation->


;;; Code:

(defvar filladapt-token-table) ;; quieten the byte compiler, from filladapt.el

(defcustom filladapt-pat-globally nil
  "`filladapt-pat' functions to apply globally when `filladapt' loads.
This is experimental."
  :group 'fill
  :type '(repeat function)
  :set (lambda (sym val)
         (custom-set-default sym val)
         (run-hook-with-args 'filladapt-pat-globally 'globally))
  :options '(filladapt-pat-no-numbered-bullets
             filladapt-pat-no-symbol-bullets
             filladapt-pat-no-postscript
             filladapt-pat-no-supercite
             filladapt-pat-no-citation->
             filladapt-pat-bullet-<li>
             filladapt-pat-bullet-<p>
             filladapt-pat-bullet-<!--
             filladapt-pat-bullet-pod
             filladapt-pat-version-bullet))

(defvar filladapt-pat-pending-local nil
  "A list of functions to run in this buffer when `filladapt' loads.
This is an internal part of filladapt-pat.el.")
(make-variable-buffer-local 'filladapt-pat-pending-local)

(defvar filladapt-pat-pending-global nil
  "A list of functions to run globally when `filladapt' loads.
This is an internal part of filladapt-pat.el.")

(defvar filladapt-pat-global-arg nil)

(defun filladapt-pat-after-load ()
  "Apply pending filladapt-pat setups when `filladapt' loads.
This is an internal part of filladapt-pat.el."
  (run-hook-with-args 'filladapt-pat-globally 'globally)
  (let ((filladapt-pat-global-arg 'globally))
    (run-hooks 'filladapt-pat-pending-global))
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (run-hooks 'filladapt-pat-pending-local)
      (kill-local-variable 'filladapt-pat-pending-local))))
(eval-after-load "filladapt" '(filladapt-pat-after-load))

(defun filladapt-pat-token-func (func)
  "Modify `filladapt-token-table' using FUNC.
This is an internal part of filladapt-pat.el.

FUNC is called as (FUNC filladapt-token-table) and its return
value is written back to `filladapt-token-table', buffer local.
If filladapt isn't loaded yet then FUNC is held and run when it
does."
  (if (featurep 'filladapt)
      ;; apply now
      (if filladapt-pat-global-arg
          ;; global
          (set-default 'filladapt-token-table
                       (funcall func (default-value 'filladapt-token-table)))
        ;; buffer-local
        (make-local-variable 'filladapt-token-table)
        (setq filladapt-token-table (funcall func filladapt-token-table)))

    ;; defer
    (add-to-list (if filladapt-pat-global-arg
                     'filladapt-pat-pending-global  ;; global
                   'filladapt-pat-pending-local)    ;; buffer-local
                 `(lambda () (filladapt-pat-token-func ,func)))))

(defun filladapt-pat-no-elem (elem)
  "Remove a particular ELEM entries from `filladapt-token-table'.
ELEM is a (REGEXP SYM) form, compared against entries using `equal'."
  (filladapt-pat-token-func
   ;; OR lexical-let from 'cl
   `(lambda (token-table)
      (remove ',elem token-table))))

(defun filladapt-pat-no-sym (sym)
  "Remove all entries for SYM from `filladapt-token-table'."
  (filladapt-pat-token-func
   `(lambda (token-table)
      ;; OR (require 'cl) (remove* ',sym token-table :key 'cadr)
      (apply 'append (mapcar (lambda (elem)
                               (and (not (eq ',sym (cadr elem)))
                                    (list elem)))
                             token-table)))))

(defun filladapt-pat-add (elem)
  "Add an particular ELEM entry to `filladapt-token-table'.
ELEM is a (REGEXP SYM) form.  It's appended to
`filladapt-token-table' so as to obey the comment with that
variable that its (\"^\" beginning-of-line) entry must be first."
  (filladapt-pat-token-func `(lambda (token-table)
                               (add-to-list 'token-table ',elem
                                            t)))) ;; append


;;-----------------------------------------------------------------------------
;; removing builtins

;;;###autoload
(defun filladapt-pat-no-numbered-bullets (&optional filladapt-pat-global-arg)
  ;; checkdoc-params: (filladapt-pat-global-arg)
  "No `filladapt' numbered bullet points like \"2.1\" (buffer-local)."
  (interactive)
  (filladapt-pat-no-elem '("[0-9]+\\(\\.[0-9]+\\)+[ \t]" bullet))
  (filladapt-pat-no-elem '("[0-9]+\\.[ \t]" bullet))
  (filladapt-pat-no-elem '("(?[0-9]+)[ \t]" bullet)))

;;;###autoload
(defun filladapt-pat-no-symbol-bullets (&optional filladapt-pat-global-arg)
  ;; checkdoc-params: (filladapt-pat-global-arg)
  "No `filladapt' symbol bullet points like \"*\" or \"-\" (buffer-local)."
  (interactive)
  (filladapt-pat-no-elem '("[-~*+]+[ \t]" bullet)))

;;;###autoload
(defun filladapt-pat-no-postscript (&optional filladapt-pat-global-arg)
  ;; checkdoc-params: (filladapt-pat-global-arg)
  "No `filladapt' postscript comments \"%\" (buffer-local).
This is good in Perl modes to stop a hash variable in a comment

    # the variable
    # %foo blah blah

being taken as two paragraphs of one line each because the second
has a \"%\" postscript comment."
  (interactive)
  (filladapt-pat-no-elem '("%+" postscript-comment)))
;;;###autoload
(custom-add-option 'perl-mode-hook  'filladapt-pat-no-postscript)
;;;###autoload
(custom-add-option 'cperl-mode-hook 'filladapt-pat-no-postscript)
;;;###autoload
(custom-add-option 'pod-mode-hook   'filladapt-pat-no-postscript)

;;;###autoload
(defun filladapt-pat-no-supercite (&optional filladapt-pat-global-arg)
  ;; checkdoc-params: (filladapt-pat-global-arg)
  "No `filladapt' supercite \"FOO>\" (buffer-local).
Removing supercite is good in Perl POD when markup crosses a line
break, making \"thing>\" look like a supercite.

    this is C<some
    thing> blah blah

\(See `perl-pod-gt-nobreak-p' from perl-pod-gt.el to avoid such
breaks in S<> markup, though not other markup.)

The same sort of \">\" can occur in HTML, but the supercite
pattern disallows quotes so the usual case of a tag ending with a
quoted attribute not struck,

    ... <tag
    attr=''>         <-- not matched by supercite pattern"

  (interactive)
  (filladapt-pat-no-sym 'supercite-citation))
;;;###autoload
(custom-add-option 'perl-mode-hook  'filladapt-pat-no-supercite)
;;;###autoload
(custom-add-option 'cperl-mode-hook 'filladapt-pat-no-supercite)
;;;###autoload
(custom-add-option 'pod-mode-hook   'filladapt-pat-no-supercite)

;;;###autoload
(defun filladapt-pat-no-citation-> (&optional filladapt-pat-global-arg)
  ;; checkdoc-params: (filladapt-pat-global-arg)
  "No `filladapt' email citation \">\" (buffer-local).
Usually \">\" is fine in all modes and can be good if cutting and
pasting some email into a text file or program file, but
sometimes it can mistake a greater-than sign at the start of a
line.  This function can disable it if that happens, perhaps just
interactively as a one-off. "
  (interactive)
  (filladapt-pat-no-sym 'citation->))


;;-----------------------------------------------------------------------------
;; adding more bullets

;;;###autoload
(defun filladapt-pat-bullet (regexp &optional filladapt-pat-global-arg)
  ;; checkdoc-params: (filladapt-pat-global-arg)
  "Add REGEXP as a `filladapt' bullet point (buffer-local)."
  (filladapt-pat-add (list regexp 'bullet)))

;;;###autoload
(defun filladapt-pat-bullet-<li> (&optional filladapt-pat-global-arg)
  ;; checkdoc-params: (filladapt-pat-global-arg)
  "Add <li> as a `filladapt' bullet point (buffer-local)."
  (interactive)
  (filladapt-pat-bullet "<li>[ \t]*"))
;;;###autoload
(custom-add-option 'html-mode-hook 'filladapt-pat-bullet-<li>)

;;;###autoload
(defun filladapt-pat-bullet-<p> (&optional filladapt-pat-global-arg)
  ;; checkdoc-params: (filladapt-pat-global-arg)
  "Add <li> as a `filladapt' bullet point (buffer-local)."
  (interactive)
  (filladapt-pat-bullet "<p>[ \t]*"))
;;;###autoload
(custom-add-option 'html-mode-hook 'filladapt-pat-bullet-<p>)

;;;###autoload
(defun filladapt-pat-bullet-<!-- (&optional filladapt-pat-global-arg)
  ;; checkdoc-params: (filladapt-pat-global-arg)
  "Add <!-- as a `filladapt' bullet point (buffer-local)."
  (interactive)
  (filladapt-pat-bullet "<!--[ \t]+"))
;;;###autoload
(custom-add-option 'html-mode-hook 'filladapt-pat-bullet-<!--)
;;;###autoload
(custom-add-option 'sgml-mode-hook 'filladapt-pat-bullet-<!--)

;;;###autoload
(defun filladapt-pat-bullet-pod (&optional filladapt-pat-global-arg)
  ;; checkdoc-params: (filladapt-pat-global-arg)
  "Add POD =foo as a `filladapt' bullet point (buffer-local).
This gives for instance

    =item this is an item in
          some sort of list"
  (interactive)
  (filladapt-pat-bullet "^\\(#[ \t]*\\)?=[a-z]+"))
;;;###autoload
(custom-add-option 'perl-mode-hook  'filladapt-pat-bullet-pod)
;;;###autoload
(custom-add-option 'cperl-mode-hook 'filladapt-pat-bullet-pod)
;;;###autoload
(custom-add-option 'pod-mode-hook   'filladapt-pat-bullet-pod)
;;;###autoload
(custom-add-option 'xs-mode-hook    'filladapt-pat-bullet-pod)

(defun filladapt-pat-version-bullet (&optional filladapt-pat-global-arg)
  ;; checkdoc-params: (filladapt-pat-global-arg)
  "Setup \"Version 123 - \" as a bullet point for filladapt.
This formats short version strings as

    Version 3 - some thing blah blah blah about this
                new version"
  (interactive)
  (filladapt-pat-bullet "Version [0-9]+ +- +"))

;; LocalWords: filladapt el

(provide 'filladapt-pat)

;;; filladapt-pat.el ends here
