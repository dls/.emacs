;;; xmtn-propagate.el --- manage multiple propagations for DVC backend for monotone

;; Copyright (C) 2009 Stephen Leake

;; Author: Stephen Leake
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

(eval-and-compile
  ;; these have macros we use
  (require 'xmtn-ids))

(eval-when-compile
  ;; these have functions we use
  (require 'xmtn-base)
  (require 'xmtn-conflicts))

(defvar xmtn-propagate-from-root ""
  "Buffer-local variable holding `from' root directory.")
(make-variable-buffer-local 'xmtn-propagate-from-root)
(put 'xmtn-propagate-from-root 'permanent-local t)

(defvar xmtn-propagate-to-root ""
  "Buffer-local variable holding `to' root directory.")
(make-variable-buffer-local 'xmtn-propagate-to-root)
(put 'xmtn-propagate-to-root 'permanent-local t)

(defvar xmtn-propagate-ewoc nil
  "Buffer-local ewoc for displaying propagations.
All xmtn-propagate functions operate on this ewoc.
The elements must all be of class xmtn-propagate-data.")
(make-variable-buffer-local 'xmtn-propagate-ewoc)
(put 'xmtn-propagate-ewoc 'permanent-local t)

(defstruct (xmtn-propagate-data (:copier nil))
  from-work          ; directory name relative to xmtn-propagate-from-root
  to-work            ; directory name relative to xmtn-propagate-to-root
  from-name          ; display name, in buffer and menus
  to-name            ;
  from-branch        ; branch name (assumed never changes)
  to-branch          ;
  from-session       ; mtn automate session
  to-session         ;
  need-refresh       ; nil | t; if an async process was started that invalidates state data
  from-head-rev      ; nil | mtn rev string; current head revision; nil if multiple heads
  to-head-rev        ;
  conflicts-buffer   ; *xmtn-conflicts* buffer for this propagation
  propagate-needed   ; nil | t
  from-heads         ; 'at-head | 'need-update | 'need-merge)
  to-heads           ;
  (from-local-changes
   'need-scan)       ; 'need-scan | 'need-commit | 'ok
  (to-local-changes
   'need-scan)       ;
  (conflicts
   'need-scan)       ; 'need-scan | 'need-resolve | 'need-review-resolve-internal | 'ok
  )

(defun xmtn-propagate-from-work (data)
  (concat xmtn-propagate-from-root (xmtn-propagate-data-from-work data)))

(defun xmtn-propagate-to-work (data)
  (concat xmtn-propagate-to-root (xmtn-propagate-data-to-work data)))

(defun xmtn-propagate-from-name ()
  "Display name for current `from' workspace."
  (xmtn-propagate-data-from-name (ewoc-data (ewoc-locate xmtn-propagate-ewoc))))

(defun xmtn-propagate-to-name ()
  "Display name for current `to' workspace."
  (xmtn-propagate-data-to-name (ewoc-data (ewoc-locate xmtn-propagate-ewoc))))

(defun xmtn-propagate-need-refresh (elem data)
  (setf (xmtn-propagate-data-need-refresh data) t)
  (ewoc-invalidate xmtn-propagate-ewoc elem))

(defun xmtn-propagate-printer (data)
  "Print an ewoc element."
  (if (string= (xmtn-propagate-data-from-work data)
               (xmtn-propagate-data-to-work data))
      (insert (dvc-face-add (format "%s\n" (xmtn-propagate-data-from-work data)) 'dvc-keyword))
    (insert (dvc-face-add (format "%s -> %s\n"
                                  (xmtn-propagate-data-from-work data)
                                  (xmtn-propagate-data-to-work data))
                          'dvc-keyword)))

  (if (xmtn-propagate-data-need-refresh data)
      (insert (dvc-face-add "  need refresh\n" 'dvc-conflict))

    (ecase (xmtn-propagate-data-from-local-changes data)
      (need-scan (insert "  local changes unknown " (xmtn-propagate-data-from-name data) "\n"))
      (need-commit
       (insert (dvc-face-add (concat "  need commit " (xmtn-propagate-data-from-name data) "\n")
                             'dvc-header)))
      (ok nil))

    (ecase (xmtn-propagate-data-to-local-changes data)
      (need-scan (insert "  local changes unknown " (xmtn-propagate-data-to-name data) "\n"))
      (need-commit
       (insert (dvc-face-add (concat "  need commit " (xmtn-propagate-data-to-name data) "\n")
                             'dvc-header)))
      (ok nil))

    (ecase (xmtn-propagate-data-from-heads data)
      (at-head     nil)
      (need-update
       (insert (dvc-face-add (concat "  need update " (xmtn-propagate-data-from-name data) "\n")
                             'dvc-conflict)))
      (need-merge
       (insert (dvc-face-add (concat "  need merge " (xmtn-propagate-data-from-name data) "\n")
                             'dvc-conflict))))

    (ecase (xmtn-propagate-data-to-heads data)
      (at-head     nil)
      (need-update
       (insert (dvc-face-add (concat "  need update " (xmtn-propagate-data-to-name data) "\n")
                             'dvc-conflict)))
      (need-merge
       (insert (dvc-face-add (concat "  need merge " (xmtn-propagate-data-to-name data) "\n")
                                   'dvc-conflict))))

    (if (xmtn-propagate-data-propagate-needed data)

        (if (and (eq 'at-head (xmtn-propagate-data-from-heads data))
                 (eq 'at-head (xmtn-propagate-data-to-heads data)))
            (ecase (xmtn-propagate-data-conflicts data)
              (need-scan
               (insert "conflicts need scan\n"))
              (need-resolve
               (insert (dvc-face-add "  need resolve conflicts\n" 'dvc-conflict)))
              (need-review-resolve-internal
               (insert (dvc-face-add "  need review resolve internal\n" 'dvc-header))
               (insert (dvc-face-add "  need propagate\n" 'dvc-conflict)))
              (ok
               (insert (dvc-face-add "  need propagate\n" 'dvc-conflict)))))

      (if (eq 'at-head (xmtn-propagate-data-to-heads data))
          (insert (dvc-face-add "  need clean\n" 'dvc-conflict)))
      ))
  ;; ewoc ought to do this, but it doesn't
  (redisplay))

(defun xmtn-propagate-kill-conflicts-buffer (data)
  (if (buffer-live-p (xmtn-propagate-data-conflicts-buffer data))
      (let ((buffer (xmtn-propagate-data-conflicts-buffer data)))
        (with-current-buffer buffer (save-buffer))
        (kill-buffer buffer))))

(defun xmtn-propagate-save-conflicts-buffer (data)
  (if (buffer-live-p (xmtn-propagate-data-conflicts-buffer data))
      (with-current-buffer (xmtn-propagate-data-conflicts-buffer data) (save-buffer))))

(defun xmtn-propagate-clean ()
  "Clean current workspace, delete from ewoc"
  (interactive)
  (let* ((elem (ewoc-locate xmtn-propagate-ewoc))
         (data (ewoc-data elem)))

    ;; only one conflicts file and buffer
    (xmtn-propagate-kill-conflicts-buffer data)
    (xmtn-conflicts-clean (xmtn-propagate-to-work data))

    (let ((inhibit-read-only t))
      (ewoc-delete xmtn-propagate-ewoc elem))))

(defun xmtn-propagate-cleanp ()
  "Non-nil if clean is appropriate for current workspace."
  (let ((data (ewoc-data (ewoc-locate xmtn-propagate-ewoc))))
    ;; don't check need-refresh here; allow deleting after just doing
    ;; final required action in another buffer. Or we've just started,
    ;; but the user knows it's ok.
    (and (member (xmtn-propagate-data-from-local-changes data) '(need-scan ok))
         (member (xmtn-propagate-data-to-local-changes data) '(need-scan ok))
         (not (xmtn-propagate-data-propagate-needed data))
         (member (xmtn-propagate-data-to-heads data) '(need-scan at-head)))))

(defun xmtn-propagate-do-refresh-one ()
  (interactive)
  (let* ((elem (ewoc-locate xmtn-propagate-ewoc))
         (data (ewoc-data elem)))
    (xmtn-propagate-refresh-one data (or current-prefix-arg
                                         (not (xmtn-propagate-data-need-refresh data))))
    (ewoc-invalidate xmtn-propagate-ewoc elem)))

(defun xmtn-propagate-refreshp ()
  "Non-nil if refresh is appropriate for current workspace."
  (let ((data (ewoc-data (ewoc-locate xmtn-propagate-ewoc))))
    (or (xmtn-propagate-data-need-refresh data)
        (eq 'need-scan (xmtn-propagate-data-from-local-changes data))
        (eq 'need-scan (xmtn-propagate-data-to-local-changes data)))))

(defun xmtn-propagate-update-to ()
  "Update current workspace."
  (interactive)
  (let* ((elem (ewoc-locate xmtn-propagate-ewoc))
         (data (ewoc-data elem)))
    (xmtn-propagate-need-refresh elem data)
    (xmtn--update (xmtn-propagate-to-work data)
                  (xmtn-propagate-data-to-head-rev data)
                  nil t)
    (xmtn-propagate-refresh-one data nil)
    (ewoc-invalidate xmtn-propagate-ewoc elem)))

(defun xmtn-propagate-update-from ()
  "Update current workspace."
  (interactive)
  (let* ((elem (ewoc-locate xmtn-propagate-ewoc))
         (data (ewoc-data elem)))
    (xmtn-propagate-need-refresh elem data)
    (xmtn--update (xmtn-propagate-from-work data)
                  (xmtn-propagate-data-from-head-rev data)
                  nil t)
    (xmtn-propagate-refresh-one data nil)
    (ewoc-invalidate xmtn-propagate-ewoc elem)))

(defun xmtn-propagate-propagate ()
  "Propagate current workspace."
  (interactive)
  (let* ((elem (ewoc-locate xmtn-propagate-ewoc))
         (data (ewoc-data elem)))
    (xmtn-propagate-need-refresh elem data)

    (if (not (buffer-live-p (xmtn-propagate-data-conflicts-buffer data)))
        ;; user deleted conflicts buffer after resolving conflicts; get it back
        (setf (xmtn-propagate-data-conflicts-buffer data)
              (xmtn-propagate-conflicts-buffer data)))

    (with-current-buffer (xmtn-propagate-data-conflicts-buffer data)
      (let ((xmtn-confirm-operation nil))
        (xmtn-conflicts-do-propagate (xmtn-propagate-data-to-branch data))))
    (xmtn-propagate-refresh-one data nil)
    (ewoc-invalidate xmtn-propagate-ewoc elem)))

(defun xmtn-propagate-propagatep ()
  "Non-nil if propagate is appropriate for current workspace."
  (let* ((data (ewoc-data (ewoc-locate xmtn-propagate-ewoc))))
    (and (not (xmtn-propagate-data-need-refresh data))
         (xmtn-propagate-data-propagate-needed data)
         (eq 'at-head (xmtn-propagate-data-from-heads data))
         (eq 'at-head (xmtn-propagate-data-to-heads data))
         (member (xmtn-propagate-data-conflicts data)
                 '(ok need-review-resolve-internal)))))

(defun xmtn-propagate-resolve-conflicts ()
  "Resolve conflicts for current workspace."
  (interactive)
  (let* ((elem (ewoc-locate xmtn-propagate-ewoc))
         (data (ewoc-data elem)))
    (xmtn-propagate-need-refresh elem data)
    (setf (xmtn-propagate-data-conflicts data) 'ok)
    (pop-to-buffer (xmtn-propagate-data-conflicts-buffer data))))

(defun xmtn-propagate-resolve-conflictsp ()
  "Non-nil if resolve conflicts is appropriate for current workspace."
  (let* ((data (ewoc-data (ewoc-locate xmtn-propagate-ewoc))))
    (and (not (xmtn-propagate-data-need-refresh data))
         (xmtn-propagate-data-propagate-needed data)
         (eq 'at-head (xmtn-propagate-data-from-heads data))
         (eq 'at-head (xmtn-propagate-data-to-heads data))
         (member (xmtn-propagate-data-conflicts data)
                 '(need-resolve need-review-resolve-internal)))))

(defun xmtn-propagate-status-to ()
  "Run xmtn-status on current `to' workspace."
  (interactive)
  (let* ((elem (ewoc-locate xmtn-propagate-ewoc))
         (data (ewoc-data elem)))
    (xmtn-propagate-need-refresh elem data)

    ;; can't create log-edit buffer with both conflicts and status
    ;; buffer open, and we'll be killing this as part of the refresh
    ;; anyway.
    (xmtn-propagate-kill-conflicts-buffer data)

    (setf (xmtn-propagate-data-to-local-changes data) 'ok)
    (xmtn-status (xmtn-propagate-to-work data))))

(defun xmtn-propagate-status-to-ok ()
  "Ignore local changes in current `to' workspace."
  (interactive)
  (let* ((elem (ewoc-locate xmtn-propagate-ewoc))
         (data (ewoc-data elem)))
    (setf (xmtn-propagate-data-to-local-changes data) 'ok)
    (ewoc-invalidate xmtn-propagate-ewoc elem)))

(defun xmtn-propagate-status-top ()
  "Non-nil if xmtn-status is appropriate for current `to' workspace."
  (let* ((data (ewoc-data (ewoc-locate xmtn-propagate-ewoc))))
    (and (not (xmtn-propagate-data-need-refresh data))
         (member (xmtn-propagate-data-to-local-changes data)
                 '(need-scan need-commit)))))

(defun xmtn-propagate-status-from ()
  "Run xmtn-status on current `from' workspace."
  (interactive)
  (let* ((elem (ewoc-locate xmtn-propagate-ewoc))
         (data (ewoc-data elem)))
    (xmtn-propagate-need-refresh elem data)
    (setf (xmtn-propagate-data-from-local-changes data) 'ok)
    (xmtn-status (xmtn-propagate-from-work data))))

(defun xmtn-propagate-status-from-ok ()
  "Ignore local changes in current `from' workspace."
  (interactive)
  (let* ((elem (ewoc-locate xmtn-propagate-ewoc))
         (data (ewoc-data elem)))
    (setf (xmtn-propagate-data-from-local-changes data) 'ok)
    (ewoc-invalidate xmtn-propagate-ewoc elem)))

(defun xmtn-propagate-status-fromp ()
  "Non-nil if xmtn-status is appropriate for current `from' workspace."
  (let* ((data (ewoc-data (ewoc-locate xmtn-propagate-ewoc))))
    (and (not (xmtn-propagate-data-need-refresh data))
         (member (xmtn-propagate-data-from-local-changes data)
                 '(need-scan need-commit)))))

(defun xmtn-propagate-missing-to ()
  "Run xmtn-missing on current `to' workspace."
  (interactive)
  (let* ((elem (ewoc-locate xmtn-propagate-ewoc))
         (data (ewoc-data elem)))
    (xmtn-propagate-need-refresh elem data)
    (xmtn-missing nil (xmtn-propagate-to-work data))))

(defun xmtn-propagate-missing-top ()
  "Non-nil if xmtn-missing is appropriate for current `to' workspace."
  (let* ((data (ewoc-data (ewoc-locate xmtn-propagate-ewoc))))
    (and (not (xmtn-propagate-data-need-refresh data))
         (eq 'need-update (xmtn-propagate-data-to-heads data)))))

(defun xmtn-propagate-missing-from ()
  "Run xmtn-missing on current `from' workspace."
  (interactive)
  (let* ((elem (ewoc-locate xmtn-propagate-ewoc))
         (data (ewoc-data elem)))
    (xmtn-propagate-need-refresh elem data)
    (xmtn-missing nil (xmtn-propagate-from-work data))))

(defun xmtn-propagate-missing-fromp ()
  "Non-nil if xmtn-missing is appropriate for current `from' workspace."
  (let* ((data (ewoc-data (ewoc-locate xmtn-propagate-ewoc))))
    (and (not (xmtn-propagate-data-need-refresh data))
         (eq 'need-update (xmtn-propagate-data-from-heads data)))))

(defun xmtn-propagate-heads-to ()
  "Run xmtn-heads on current `to' workspace."
  (interactive)
  (let* ((elem (ewoc-locate xmtn-propagate-ewoc))
         (data (ewoc-data elem))
         (default-directory (xmtn-propagate-to-work data)))
    (xmtn-propagate-need-refresh elem data)
    (xmtn-view-heads-revlist)))

(defun xmtn-propagate-heads-top ()
  "Non-nil if xmtn-heads is appropriate for current `to' workspace."
  (let* ((data (ewoc-data (ewoc-locate xmtn-propagate-ewoc))))
    (and (not (xmtn-propagate-data-need-refresh data))
         (eq 'need-merge (xmtn-propagate-data-to-heads data)))))

(defun xmtn-propagate-heads-from ()
  "Run xmtn-heads on current `from' workspace."
  (interactive)
  (let* ((elem (ewoc-locate xmtn-propagate-ewoc))
         (data (ewoc-data elem))
         (default-directory (xmtn-propagate-from-work data)))
    (xmtn-propagate-need-refresh elem data)
    (xmtn-view-heads-revlist)))

(defun xmtn-propagate-heads-fromp ()
  "Non-nil if xmtn-heads is appropriate for current `from' workspace."
  (let* ((data (ewoc-data (ewoc-locate xmtn-propagate-ewoc))))
    (and (not (xmtn-propagate-data-need-refresh data))
         (eq 'need-merge (xmtn-propagate-data-from-heads data)))))

(defvar xmtn-propagate-actions-map
  (let ((map (make-sparse-keymap "actions")))
    (define-key map [?c]  '(menu-item "c) clean/delete"
                                      xmtn-propagate-clean
                                      :visible (xmtn-propagate-cleanp)))
    (define-key map [?g]  '(menu-item "g) refresh"
                                      xmtn-propagate-do-refresh-one
                                      :visible (xmtn-propagate-refreshp)))
    (define-key map [?b]  '(menu-item "b) propagate"
                                      xmtn-propagate-propagate
                                      :visible (xmtn-propagate-propagatep)))
    (define-key map [?a]  '(menu-item "a) resolve conflicts"
                                      xmtn-propagate-resolve-conflicts
                                      :visible (xmtn-propagate-resolve-conflictsp)))
    (define-key map [?9]  '(menu-item (concat "9) ignore local changes " (xmtn-propagate-to-name))
                                      xmtn-propagate-status-to-ok
                                      :visible (xmtn-propagate-status-top)))
    (define-key map [?8]  '(menu-item (concat "8) ignore local changes " (xmtn-propagate-from-name))
                                      xmtn-propagate-status-from-ok
                                      :visible (xmtn-propagate-status-fromp)))
    (define-key map [?7]  '(menu-item (concat "7) dvc-missing " (xmtn-propagate-to-name))
                                      xmtn-propagate-missing-to
                                      :visible (xmtn-propagate-missing-top)))
    (define-key map [?6]  '(menu-item (concat "6) dvc-missing " (xmtn-propagate-from-name))
                                      xmtn-propagate-missing-from
                                      :visible (xmtn-propagate-missing-fromp)))
    (define-key map [?5]  '(menu-item (concat "5) update " (xmtn-propagate-to-name))
                                      xmtn-propagate-update-to
                                      :visible (xmtn-propagate-missing-top)))
    (define-key map [?4]  '(menu-item (concat "4) update " (xmtn-propagate-from-name))
                                      xmtn-propagate-update-from
                                      :visible (xmtn-propagate-missing-fromp)))
    (define-key map [?3]  '(menu-item (concat "3) commit " (xmtn-propagate-to-name))
                                      xmtn-propagate-status-to
                                      :visible (xmtn-propagate-status-top)))
    (define-key map [?2]  '(menu-item (concat "2) commit " (xmtn-propagate-from-name))
                                      xmtn-propagate-status-from
                                      :visible (xmtn-propagate-status-fromp)))
    (define-key map [?1]  '(menu-item (concat "1) xmtn-heads " (xmtn-propagate-to-name))
                                      xmtn-propagate-heads-to
                                      :visible (xmtn-propagate-heads-top)))
    (define-key map [?0]  '(menu-item (concat "0) xmtn-heads " (xmtn-propagate-from-name))
                                      xmtn-propagate-heads-from
                                      :visible (xmtn-propagate-heads-fromp)))
    map)
  "Keyboard menu keymap used to manage propagates.")

(dvc-make-ewoc-next xmtn-propagate-next xmtn-propagate-ewoc)
(dvc-make-ewoc-prev xmtn-propagate-prev xmtn-propagate-ewoc)

(defvar xmtn-propagate-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\M-d" xmtn-propagate-actions-map)
    (define-key map [?g]  'xmtn-propagate-refresh)
    (define-key map [?n]  'xmtn-propagate-next)
    (define-key map [?p]  'xmtn-propagate-prev)
    (define-key map [?q]  (lambda () (interactive) (kill-buffer (current-buffer))))
    map)
  "Keymap used in `xmtn-propagate-mode'.")

(define-derived-mode xmtn-propagate-mode nil "xmtn-propagate"
  "Major mode to propagate multiple workspaces."
  (setq dvc-buffer-current-active-dvc 'xmtn)
  (setq buffer-read-only nil)

  ;; don't do normal clean up stuff
  (set (make-local-variable 'before-save-hook) nil)
  (set (make-local-variable 'write-file-functions) nil)

  (dvc-install-buffer-menu)
  (setq buffer-read-only t)
  (buffer-disable-undo)
  (set-buffer-modified-p nil)
  (xmtn-propagate-refresh)
  (xmtn-propagate-next nil t))

(defun xmtn-propagate-needed (data)
  "t if DATA needs propagate."
  (let ((result t)
        (from-work (xmtn-propagate-from-work data))
        (from-head-rev (xmtn-propagate-data-from-head-rev data))
        (to-head-rev   (xmtn-propagate-data-to-head-rev data)))

    (if (or (not from-head-rev)
            (not to-head-rev))
        ;; multiple heads; can't propagate
        (setq result nil)

      ;; 1) to branched off earlier, and propagate is needed
      ;; 2) propagate was just done but required no changes; no propagate needed
      ;;
      (if (string= from-head-rev to-head-rev)
          ;; case 2
          (setq result nil)
        (let ((descendents (xmtn-automate-simple-command-output-lines from-work (list "descendents" from-head-rev)))
              done)
          (if (not descendents)
              ;; case 1
              (setq result t)
            (while (and descendents (not done))
              (if (string= to-head-rev (car descendents))
                  (progn
                    (setq result nil)
                    (setq done t)))
              (setq descendents (cdr descendents)))))))
    result
  ))

(defun xmtn-propagate-conflicts-buffer (data)
  "Return a conflicts buffer for FROM-WORK, TO-WORK (absolute paths)."
  (let ((from-work (xmtn-propagate-from-work data))
        (from-head-rev (xmtn-propagate-data-from-head-rev data))
        (to-work (xmtn-propagate-to-work data))
        (to-head-rev (xmtn-propagate-data-to-head-rev data)))

    (or (dvc-get-buffer 'xmtn 'conflicts to-work)
        (let ((default-directory to-work))
          (if (not (file-exists-p "_MTN/conflicts"))
              (progn
                ;; create conflicts file
                (xmtn-conflicts-save-opts
                 from-work
                 to-work
                 (xmtn-propagate-data-from-branch data)
                 (xmtn-propagate-data-to-branch data))
                (dvc-run-dvc-sync
                 'xmtn
                 (list "conflicts" "store" from-head-rev to-head-rev)

                 :error (lambda (output error status arguments)
                          (pop-to-buffer error)))))
          ;; create conflicts buffer
          (save-excursion
            (let ((dvc-switch-to-buffer-first nil))
              (xmtn-conflicts-review to-work)
              (current-buffer)))))))

(defun xmtn-propagate-conflicts (data)
  "Return value for xmtn-propagate-data-conflicts for DATA."

  (if (not (buffer-live-p (xmtn-propagate-data-conflicts-buffer data)))
      ;; user may have deleted conflicts buffer after resolving
      ;; conflicts; don't throw that away.
      (setf (xmtn-propagate-data-conflicts-buffer data)
            (xmtn-propagate-conflicts-buffer data)))

  (let ((revs-current
         (with-current-buffer (xmtn-propagate-data-conflicts-buffer data)
           (and (string= (xmtn-propagate-data-from-head-rev data) xmtn-conflicts-left-revision)
                (string= (xmtn-propagate-data-to-head-rev data) xmtn-conflicts-right-revision)))))
    (if revs-current
        (with-current-buffer (xmtn-propagate-data-conflicts-buffer data)
          (xmtn-conflicts-update-counts)
          (save-buffer))

      ;; else recreate conflicts
      (xmtn-propagate-kill-conflicts-buffer data)

      (xmtn-conflicts-clean (xmtn-propagate-to-work data))

      (setf (xmtn-propagate-data-conflicts-buffer data)
            (xmtn-propagate-conflicts-buffer data))
      )

    (with-current-buffer (xmtn-propagate-data-conflicts-buffer data)
      (if (= xmtn-conflicts-total-count xmtn-conflicts-resolved-count)
          (if (< 0 xmtn-conflicts-resolved-internal-count)
              'need-review-resolve-internal
            'ok)
        'need-resolve))))

(defun xmtn-propagate-refresh-one (data refresh-local-changes)
  "Refresh DATA."
  (let ((from-work (xmtn-propagate-from-work data))
        (to-work (xmtn-propagate-to-work data)))

    (dvc-trace "xmtn-propagate-refresh-one: %s" from-work)

    (if refresh-local-changes
        (progn
          (setf (xmtn-propagate-data-from-local-changes data) 'need-scan)
          (setf (xmtn-propagate-data-to-local-changes data) 'need-scan)))

    (let ((heads (xmtn--heads from-work (xmtn-propagate-data-from-branch data)))
          (from-base-rev (xmtn--get-base-revision-hash-id-or-null from-work)))
      (case (length heads)
        (1
         (setf (xmtn-propagate-data-from-head-rev data) (nth 0 heads))
         (if (string= (xmtn-propagate-data-from-head-rev data) from-base-rev)
               (setf (xmtn-propagate-data-from-heads data) 'at-head)
             (setf (xmtn-propagate-data-from-heads data) 'need-update)))
        (t
         (setf (xmtn-propagate-data-from-head-rev data) nil)
         (setf (xmtn-propagate-data-from-heads data) 'need-merge))))

    (let ((heads (xmtn--heads to-work (xmtn-propagate-data-to-branch data)))
          (to-base-rev (xmtn--get-base-revision-hash-id-or-null to-work)))
      (case (length heads)
        (1
         (setf (xmtn-propagate-data-to-head-rev data) (nth 0 heads))
         (if (string= (xmtn-propagate-data-to-head-rev data) to-base-rev)
               (setf (xmtn-propagate-data-to-heads data) 'at-head)
             (setf (xmtn-propagate-data-to-heads data) 'need-update)))
        (t
         (setf (xmtn-propagate-data-to-head-rev data) nil)
         (setf (xmtn-propagate-data-to-heads data) 'need-merge))))

    (setf (xmtn-propagate-data-propagate-needed data)
          (xmtn-propagate-needed data))

    (if (or refresh-local-changes
            (xmtn-propagate-data-propagate-needed data))
        ;; these checks are slow, so don't do them if they probably are not needed.
        (progn
          (ecase (xmtn-propagate-data-from-local-changes data)
            ((need-scan need-commit)
             (setf (xmtn-propagate-data-from-local-changes data) (xmtn-automate-local-changes from-work)))
            (ok nil))

          (ecase (xmtn-propagate-data-to-local-changes data)
            ((need-scan need-commit)
             (setf (xmtn-propagate-data-to-local-changes data) (xmtn-automate-local-changes to-work)))
            (ok nil))))

    (if (xmtn-propagate-data-propagate-needed data)
        (progn
          (if refresh-local-changes
              (progn
                (xmtn-propagate-kill-conflicts-buffer data)
                (xmtn-conflicts-clean (xmtn-propagate-to-work data))))

          (setf (xmtn-propagate-data-conflicts data)
                (xmtn-propagate-conflicts data)))

      ;; can't compute conflicts if propagate not needed
      (setf (xmtn-propagate-data-conflicts data) 'need-scan))

    (setf (xmtn-propagate-data-need-refresh data) nil))

  ;; return non-nil to refresh display as we go along
  t)

(defun xmtn-propagate-refresh ()
  "Refresh status of each ewoc element. With prefix arg, reset local changes status to `unknown'."
  (interactive)
  (ewoc-map 'xmtn-propagate-refresh-one xmtn-propagate-ewoc current-prefix-arg)
  ;; leaves point at (point-min)
  (xmtn-propagate-next t)
  (message "done"))

(defun xmtn-propagate-make-data (from-workspace to-workspace from-name to-name)
  "FROM-WORKSPACE, TO-WORKSPACE are relative names"
    (let* ((from-work (concat xmtn-propagate-from-root from-workspace))
           ;; cached sessions not working (yet)
           ;;(from-session (xmtn-automate-cache-session from-work))
           (to-work (concat xmtn-propagate-to-root to-workspace))
           ;;(to-session (xmtn-automate-cache-session to-work))
           )

      (ewoc-enter-last
       xmtn-propagate-ewoc
       (make-xmtn-propagate-data
        :from-work from-workspace
        :to-work to-workspace
        :from-name from-name
        :to-name to-name
        :from-branch (xmtn--tree-default-branch from-work)
        :to-branch (xmtn--tree-default-branch to-work)
        :from-session nil ;; from-session
        :to-session nil ;; to-session
        :need-refresh t))))

;;;###autoload
(defun xmtn-propagate-multiple (from-dir to-dir &optional workspaces)
  "Show all actions needed to propagate projects under FROM-DIR
to TO-DIR. WORKSPACES (default nil) is a list of workspaces
common to from-dir and to-dir; if nil, the directories are
scanned and all common ones found are used."
  (interactive "DPropagate all from (root directory): \nDto (root directory): ")
  (setq from-dir (substitute-in-file-name from-dir))
  (setq to-dir (substitute-in-file-name to-dir))
  (let ((from-workspaces (or workspaces
                             (xmtn--filter-non-dir from-dir)))
        (to-workspaces (or workspaces
                           (xmtn--filter-non-dir to-dir))))

    (pop-to-buffer (get-buffer-create "*xmtn-propagate*"))
    (setq xmtn-propagate-from-root (file-name-as-directory from-dir))
    (setq xmtn-propagate-to-root (file-name-as-directory to-dir))
    (setq xmtn-propagate-ewoc (ewoc-create 'xmtn-propagate-printer))
    (let ((inhibit-read-only t)) (delete-region (point-min) (point-max)))
    (ewoc-set-hf
     xmtn-propagate-ewoc
     (concat
      (format "From root : %s\n" xmtn-propagate-from-root)
      (format "  To root : %s\n" xmtn-propagate-to-root)
      )
     "")
    (dolist (workspace from-workspaces)
      (if (member workspace to-workspaces)
          (xmtn-propagate-make-data
           workspace
           workspace
           (file-name-nondirectory (directory-file-name xmtn-propagate-from-root))
           (file-name-nondirectory (directory-file-name xmtn-propagate-to-root)))))
    (redisplay)
    (xmtn-propagate-mode)))

;;;###autoload
(defun xmtn-propagate-one (from-work to-work)
  "Show all actions needed to propagate FROM-WORK to TO-WORK."
  (interactive "DPropagate all from (workspace): \nDto (workspace): ")
  (setq from-work (substitute-in-file-name from-work))
  (setq to-work (substitute-in-file-name to-work))
  (let ((default-directory to-work)
        (from-session (xmtn-automate-cache-session from-work))
        (to-session (xmtn-automate-cache-session to-work)))
    (pop-to-buffer (get-buffer-create "*xmtn-propagate*"))
    ;; default-directory is wrong if buffer is reused
    (setq default-directory to-work)
    (setq xmtn-propagate-from-root (expand-file-name (concat (file-name-as-directory from-work) "../")))
    (setq xmtn-propagate-to-root (expand-file-name (concat (file-name-as-directory to-work) "../")))
    (setq xmtn-propagate-ewoc (ewoc-create 'xmtn-propagate-printer))
    (let ((inhibit-read-only t)) (delete-region (point-min) (point-max)))
    (ewoc-set-hf
     xmtn-propagate-ewoc
     (concat
      (format "From root : %s\n" xmtn-propagate-from-root)
      (format "  To root : %s\n" xmtn-propagate-to-root)
      )
     "")
    (xmtn-propagate-make-data
     (file-name-nondirectory (directory-file-name from-work))
     (file-name-nondirectory (directory-file-name to-work))
     (file-name-nondirectory (directory-file-name from-work))
     (file-name-nondirectory (directory-file-name to-work)))
    (xmtn-propagate-mode)))

(provide 'xmtn-propagate)

;; end of file
