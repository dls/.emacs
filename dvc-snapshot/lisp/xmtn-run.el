;;; xmtn-run.el --- Functions for runnning monotone commands

;; Copyright (C) 2008 - 2009 Stephen Leake
;; Copyright (C) 2006, 2007 Christian M. Ohler

;; Author: Christian M. Ohler
;; Keywords: tools

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin St, Fifth Floor,
;; Boston, MA  02110-1301  USA.

;;; Commentary:

;; This file provides functions for running monotone commands.  See
;; xmtn-automate.el for more sophisticated access to monotone's
;; automate interface.

;;; Code:

;;; There are some notes on the design of xmtn in
;;; docs/xmtn-readme.txt.

(eval-and-compile
  (require 'cl)
  (require 'dvc-unified)
  (when (featurep 'xemacs)
    (condition-case nil
        (require 'un-define)
      (error nil)))
  (require 'xmtn-base))

(define-coding-system-alias 'xmtn--monotone-normal-form 'utf-8-unix)

(defun* xmtn--run-command-sync (root arguments &rest dvc-run-keys &key)
  (xmtn--check-cached-command-version)
  (let ((default-directory (file-truename (or root default-directory))))
    (dvc-run-dvc-sync
     'xmtn
     `(,@xmtn-additional-arguments
       ;; We don't pass the --root argument here; it is not
       ;; necessary since default-directory is set, and it
       ;; confuses the Cygwin version of mtn when run with a
       ;; non-Cygwin Emacs.
       ,@arguments)
     dvc-run-keys)))

;;; The `dvc-run-dvc-*' functions use `call-process', which, for some
;;; reason, spawns the subprocess with a working directory with all
;;; symlinks expanded.  (Or maybe it's the shell that expands the
;;; symlinks.)  If the path to the root directory looks different from
;;; the current working directory, monotone rejects it even if it is
;;; the same via symlinks.  Therefore, we need to resolve symlinks
;;; here in strategic places.  Hence the calls to `file-truename'.

(defun* xmtn--run-command-async (root arguments &rest dvc-run-keys &key)
  (xmtn--check-cached-command-version)
  (let ((default-directory (file-truename (or root default-directory))))
    (apply #'dvc-run-dvc-async
           'xmtn
           `(,@xmtn-additional-arguments
             ;; We don't pass the --root argument here; it is not
             ;; necessary since default-directory is set, and it
             ;; confuses the Cygwin version of mtn when run with a
             ;; non-Cygwin Emacs.
             ,@arguments)
           dvc-run-keys)))

(defun xmtn--command-output-lines (root arguments)
  "Run mtn in ROOT with ARGUMENTS and return its output as a list of strings."
  (xmtn--check-cached-command-version)
  (let ((accu (list)))
    (let ((default-directory (file-truename (or root default-directory))))
      (dvc-run-dvc-sync
       'xmtn
       `(,@xmtn-additional-arguments
         ,@(if root `(,(concat "--root=" (file-truename root))))
         ,@arguments)
       :finished (lambda (output error status arguments)
                   (with-current-buffer output
                     (save-excursion
                       (goto-char (point-min))
                       (while (not (eobp))
                         (push (buffer-substring-no-properties
                                (point)
                                (progn (end-of-line) (point)))
                               accu)
                         (forward-line 1)))))))
    (setq accu (nreverse accu))
    accu))

(defun xmtn--command-output-line (root arguments)
  "Run mtn in ROOT with ARGUMENTS and return the one line of output as string.

Signals an error if more (or fewer) than one line is output."
  (let ((lines (xmtn--command-output-lines root arguments)))
    (unless (eql (length lines) 1)
      (error "Expected precisely one line of output from monotone, got %s: %s %S"
             (length lines)
             xmtn-executable
             arguments))
    (first lines)))

(defconst xmtn--minimum-required-command-version '(0 45))

(defun xmtn--have-no-ignore ()
  "Non-nil if mtn automate inventory supports --no-ignore, --no-unknown, --no-unchanged options."
  (>= (xmtn-dvc-automate-version) 7.0))

(defvar xmtn--*cached-command-version* nil)
(defvar xmtn--*command-version-cached-for-executable* nil)

(defun xmtn--clear-command-version-cache ()
  (setq xmtn--*command-version-cached-for-executable* nil
        ;; This is redundant but neater.
        xmtn--*cached-command-version* nil))

(defun xmtn--cached-command-version ()
  (if (equal xmtn--*command-version-cached-for-executable* xmtn-executable)
      xmtn--*cached-command-version*
    (let ((executable xmtn-executable))
      (prog1 (setq xmtn--*cached-command-version* (xmtn--command-version
                                                   executable))
        (setq xmtn--*command-version-cached-for-executable* executable)
        (xmtn--check-cached-command-version)))))

(defun xmtn--command-version (executable)
  "Return EXECUTABLE's version as a list (MAJOR MINOR REVISION VERSION-STRING).

VERSION-STRING is the string printed by mtn --version (with no
trailing newline).  MAJOR and MINOR are integers, a parsed
representation of the version number.  REVISION is the revision
id."
  (let (
        ;; Cache a fake version number to avoid infinite mutual
        ;; recursion.
        (xmtn--*cached-command-version*
         (append xmtn--minimum-required-command-version
                 '("xmtn-dummy" "xmtn-dummy")))
        (xmtn--*command-version-cached-for-executable* executable)
        (xmtn-executable executable))
    (let ((string (xmtn--command-output-line nil '("--version"))))
      (unless (string-match
               (concat "\\`monotone \\([0-9]+\\)\\.\\([0-9]+\\)\\(dev\\)?"
                       " (base revision: \\(unknown\\|\\([0-9a-f]\\{40\\}\\)\\))\\'")
               string)
        (error (concat "Version output from monotone --version"
                       " did not match expected pattern: %S")
               string))
      (let ((major (parse-integer string (match-beginning 1) (match-end 1)))
            (minor (parse-integer string (match-beginning 2) (match-end 2)))
            (revision (match-string 4 string)))
        (list major minor revision string)))))

(defun xmtn--check-cached-command-version ()
  (let ((minimum-version xmtn--minimum-required-command-version))
    (destructuring-bind (major minor revision string)
        (xmtn--cached-command-version)
      (unless (or (> major (car minimum-version))
                  (and (= major (car minimum-version))
                       (>= minor (cadr minimum-version))))
        ;; Clear cache now since the user is somewhat likely to
        ;; upgrade mtn (or change the value of `xmtn-executable')
        ;; after this message.
        (xmtn--clear-command-version-cache)
        (error (concat "xmtn does not work with mtn versions below %s.%s"
                       " (%s is %s)")
               (car minimum-version) (cadr minimum-version)
               xmtn-executable string)))
    nil))

;;;###autoload
(defun xmtn-check-command-version ()
  "Check and display the version identifier of the mtn command.

This command resets xmtn's command version cache."
  (interactive)
  (xmtn--clear-command-version-cache)
  (destructuring-bind (major minor revision version-string)
      (xmtn--cached-command-version)
    (let* ((latest (xmtn--latest-mtn-release))
           (latest-major (first latest))
           (latest-minor (second latest)))
      (if (eval `(xmtn--version-case
                  ((and (= ,latest-major latest-minor)
                        (mainline> latest-major latest-minor))
                   t)
                  (t
                   nil)))
          (message "%s (xmtn considers this version newer than %s.%s)"
                   version-string major minor)
        (message "%s" version-string))))
  nil)

(defun xmtn--make-version-check-form (version-var condition)
  ;; The expression (mainline> X Y) matches all command versions
  ;; strictly newer than X.Y, and, if X.Y is the latest version
  ;; according to (xmtn--latest-mtn-release), command versions that
  ;; report version X.Y with a revision ID different from what
  ;; (xmtn--latest-mtn-release) returns.  This is a kludge to attempt
  ;; to distinguish the latest mtn release from the current
  ;; bleeding-edge ("mainline") version.  (Bleeding-edge mtn versions
  ;; always report a version equal to the last release, while they
  ;; generally have syntax and semantics that match the upcoming
  ;; release; i.e., their syntax and semantics don't match the version
  ;; number they report.)
  (case condition
    ((t) `t)
    ((nil) `nil)
    (t
     (let ((operator (car condition))
           (arguments (cdr condition)))
       (ecase operator
         ((< <= > >= = /= mainline>)
          (let ((target-version arguments))
            (assert (eql (length arguments) 2))
            (ecase operator
              ((=)
               `(and (= (car ,version-var) ,(car target-version))
                     (= (cadr ,version-var) ,(cadr target-version))))
              ((< >)
               `(or (,operator (car ,version-var) ,(car target-version))
                    (and
                     (= (car ,version-var) ,(car target-version))
                     (,operator (cadr ,version-var) ,(cadr target-version)))))
              ((mainline>)
               `(or (> (car ,version-var) ,(car target-version))
                    (and (= (car ,version-var) ,(car target-version))
                         (or (> (cadr ,version-var) ,(cadr target-version))
                             (and (= (cadr ,version-var) ,(cadr target-version))
                                  (let ((-latest- (xmtn--latest-mtn-release)))
                                    (and (= (car -latest-) ,(car target-version))
                                         (= (cadr -latest-)
                                            ,(cadr target-version))
                                         (not (equal (caddr ,version-var)
                                                     (caddr -latest-))))))))))
              ((/= <= >=)
               (let ((negated-operator (ecase operator
                                         (/= '=)
                                         (<= '>)
                                         (>= '<))))
                 `(not ,(xmtn--make-version-check-form version-var
                                                       `(,negated-operator
                                                         ,@arguments))))))))
         ((not)
          (assert (eql (length arguments) 1))
          `(not ,(xmtn--make-version-check-form version-var (first arguments))))
         ((and or)
          `(,operator
            ,@(loop for subform in arguments
                    collect
                    (xmtn--make-version-check-form version-var subform)))))))))

(defun xmtn--signal-unsupported-version (version supported-conditions)
  (error "Operation only implemented for monotone versions matching %S"
         ;; This message is probably not very helpful to users who
         ;; don't know xmtn's internals.
         `(or ,@supported-conditions)))

(defmacro* xmtn--version-case (&body clauses)
  (let ((version (gensym)))
    `(let ((,version (xmtn--cached-command-version)))
       (cond ,@(loop for (condition . body) in clauses
                     collect `(,(xmtn--make-version-check-form version
                                                               condition)
                               ,@body))
             (t (xmtn--signal-unsupported-version
                 ,version
                 ',(loop for (condition . nil) in clauses
                         collect condition)))))))

(defun xmtn--latest-mtn-release ()
  ;; Version number and revision id of the latest mtn release at the
  ;; time of this xmtn release.
  '(0 35 "f92dd754bf5c1e6eddc9c462b8d68691cfeb7f8b"))

(provide 'xmtn-run)

;;; xmtn-run.el ends here
