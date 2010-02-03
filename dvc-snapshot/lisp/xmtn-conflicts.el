;;; xmtn-conflicts.el --- conflict resolution for DVC backend for monotone

;; Copyright (C) 2008 - 2009 Stephen Leake

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
  (require 'cl)
  (require 'dvc-utils)
  (require 'xmtn-automate)
  (require 'xmtn-basic-io)
  (require 'xmtn-ids)
  (require 'xmtn-run))

(eval-when-compile
  ;; these have functions we use
  (require 'dired))

(defvar xmtn-conflicts-left-revision ""
  "Buffer-local variable holding left revision id.")
(make-variable-buffer-local 'xmtn-conflicts-left-revision)

(defvar xmtn-conflicts-right-revision ""
  "Buffer-local variable holding right revision id.")
(make-variable-buffer-local 'xmtn-conflicts-right-revision)

(defvar xmtn-conflicts-left-work ""
  "Buffer-local variable holding left workspace root.")
(make-variable-buffer-local 'xmtn-conflicts-left-work)
(put 'xmtn-conflicts-left-work 'permanent-local t)

(defvar xmtn-conflicts-right-work ""
  "Buffer-local variable holding right workspace root.")
(make-variable-buffer-local 'xmtn-conflicts-right-work)
(put 'xmtn-conflicts-right-work 'permanent-local t)

(defvar xmtn-conflicts-left-root ""
  "Buffer-local variable holding left resolution root directory
  name; relative to workspace root.")
(make-variable-buffer-local 'xmtn-conflicts-left-root)

(defvar xmtn-conflicts-right-root ""
  "Buffer-local variable holding right resolution root directory
  name; relative to workspace root.")
(make-variable-buffer-local 'xmtn-conflicts-right-root)

(defvar xmtn-conflicts-left-branch ""
  "Buffer-local variable holding left resolution branch.")
(make-variable-buffer-local 'xmtn-conflicts-left-branch)
(put 'xmtn-conflicts-left-branch 'permanent-local t)

(defvar xmtn-conflicts-right-branch ""
  "Buffer-local variable holding right resolution branch.")
(make-variable-buffer-local 'xmtn-conflicts-right-branch)
(put 'xmtn-conflicts-right-branch 'permanent-local t)

(defvar xmtn-conflicts-ancestor-revision ""
  "Buffer-local variable holding ancestor revision id.")
(make-variable-buffer-local 'xmtn-conflicts-ancestor-revision-spec)

(defvar xmtn-conflicts-total-count nil
  "Total count of conflicts.")
(make-variable-buffer-local 'xmtn-conflicts-total-count)

(defvar xmtn-conflicts-resolved-count nil
  "Count of resolved conflicts.")
(make-variable-buffer-local 'xmtn-conflicts-resolved-count)

(defvar xmtn-conflicts-resolved-internal-count nil
  "Count of resolved-internal conflicts.")
(make-variable-buffer-local 'xmtn-conflicts-resolved-internal-count)

(defvar xmtn-conflicts-output-buffer nil
  "Buffer to write basic-io to, when saving a conflicts buffer.")
(make-variable-buffer-local 'xmtn-conflicts-output-buffer)

(defvar xmtn-conflicts-current-conflict-buffer nil
  "Global variable for use in ediff quit hook.")
;; xmtn-conflicts-current-conflict-buffer cannot be buffer local,
;; because ediff leaves the merge buffer active.

(defvar xmtn-conflicts-ediff-quit-info nil
  "Stuff used by ediff quit hook.")
(make-variable-buffer-local 'xmtn-conflicts-ediff-quit-info)

(defstruct (xmtn-conflicts-conflict
            (:copier nil))
  ;; not worth splitting this into a type hierarchy; differences are
  ;; minor some fields are nil for some conflict types
  ;; single file conflicts only set left_resolution

  conflict_type ;; 'content | 'duplicate_name | 'orphaned_node
  ancestor_name
  ancestor_file_id
  left_type
  left_name
  left_file_id
  right_type
  right_name
  right_file_id
  left_resolution
  right_resolution)

(defun xmtn-conflicts-printer (conflict)
  "Print an ewoc element; CONFLICT must be of type xmtn-conflicts-conflict."
  (ecase (xmtn-conflicts-conflict-conflict_type conflict)
    ('content
     (insert (dvc-face-add "content\n" 'dvc-keyword))
     (insert "ancestor:   ")
     (insert (xmtn-conflicts-conflict-ancestor_name conflict))
     (insert "\n")
     (insert "left:       ")
     (insert (xmtn-conflicts-conflict-left_name conflict))
     (insert "\n")
     (insert "right:      ")
     (insert (xmtn-conflicts-conflict-right_name conflict))
     (insert "\n")
     (insert "resolution: ")
     (insert (format "%s" (xmtn-conflicts-conflict-left_resolution conflict)))
     (insert "\n")
     )
    ('duplicate_name
     (insert (dvc-face-add "duplicate_name\n" 'dvc-keyword))
     (insert "left_type:        ")
     (insert (xmtn-conflicts-conflict-left_type conflict))
     (insert "\n")
     (insert "left:             ")
     (insert (xmtn-conflicts-conflict-left_name conflict))
     (insert "\n")
     (insert "right_type:       ")
     (insert (xmtn-conflicts-conflict-right_type conflict))
     (insert "\n")
     (insert "right:            ")
     (insert (xmtn-conflicts-conflict-right_name conflict))
     (insert "\n")
     (insert "left resolution:  ")
     (insert (format "%s" (xmtn-conflicts-conflict-left_resolution conflict)))
     (insert "\n")
     (insert "right resolution: ")
     (insert (format "%s" (xmtn-conflicts-conflict-right_resolution conflict)))
     (insert "\n")
     )
    ('orphaned_node
     (insert (dvc-face-add "orphaned_node\n" 'dvc-keyword))
     (insert "ancestor:   ")
     (insert (xmtn-conflicts-conflict-ancestor_name conflict))
     (insert "\n")
     (insert "left:       ")
     (insert (xmtn-conflicts-conflict-left_type conflict))
     (insert " ")
     (if (xmtn-conflicts-conflict-left_name conflict) (insert (xmtn-conflicts-conflict-left_name conflict)))
     (insert "\n")
     (insert "right:      ")
     (insert (xmtn-conflicts-conflict-right_type conflict))
     (insert " ")
     (if (xmtn-conflicts-conflict-right_name conflict) (insert (xmtn-conflicts-conflict-right_name conflict)))
     (insert "\n")
     (insert "resolution: ")
     (insert (format "%s" (xmtn-conflicts-conflict-left_resolution conflict)))
     (insert "\n")
     )
    ))

(defvar xmtn-conflicts-ewoc nil
  "Buffer-local ewoc for displaying conflicts.
All xmtn-conflicts functions operate on this ewoc.
The elements must all be of type xmtn-conflicts-conflict.")
(make-variable-buffer-local 'xmtn-conflicts-ewoc)

(defun xmtn-conflicts-parse-header ()
  "Fill `xmtn-conflicts-left-revision', `xmtn-conflicts-left-root',
`xmtn-conflicts-right-revision', `xmtn-conflicts-right-root'
`xmtn-conflicts-ancestor-revision' with data from conflict
header."
  ;;     left [9a019f3a364416050a8ff5c05f1e44d67a79e393]
  ;;    right [426509b2ae07b0da1472ecfd8ecc25f261fd1a88]
  ;; ancestor [dc4518d417c47985eb2cfdc2d36c7bd4c450d626]
  ;;
  ;; ancestor is not output if left is ancestor of right or vice-versa
  (setq xmtn-conflicts-ancestor-revision nil)
  (xmtn-basic-io-check-line "left" (setq xmtn-conflicts-left-revision (cadar value)))
  (xmtn-basic-io-check-line "right" (setq xmtn-conflicts-right-revision (cadar value)))
  (xmtn-basic-io-optional-line "ancestor"
    (setq xmtn-conflicts-ancestor-revision (cadar value)))
  (xmtn-basic-io-check-empty)

  ;; xmtn-conflicts-left-branch xmtn-conflicts-right-branch set by xmtn-conflicts-load-opts

  (if (string= xmtn-conflicts-left-branch xmtn-conflicts-right-branch)
      (progn
        (setq xmtn-conflicts-left-root "_MTN/resolutions/left")
        (setq xmtn-conflicts-right-root "_MTN/resolutions/right"))
    (progn
      (setq xmtn-conflicts-left-root (concat "_MTN/resolutions/" xmtn-conflicts-left-branch))
      (setq xmtn-conflicts-right-root (concat "_MTN/resolutions/" xmtn-conflicts-right-branch))))
  (setq xmtn-conflicts-total-count 0)
  (setq xmtn-conflicts-resolved-count 0)
  (setq xmtn-conflicts-resolved-internal-count 0)
  )

(defun xmtn-conflicts-parse-content-conflict ()
  "Fill an ewoc entry with data from content conflict stanza."
  ;;         conflict content
  ;;        node_type "file"
  ;;    ancestor_name "1553/gds-hardware-bus_1553-iru_honeywell-user_guide-symbols.tex"
  ;; ancestor_file_id [d1eee768379694a59b2b015dd59a61cf67505182]
  ;;        left_name "1553/gds-hardware-bus_1553-iru_honeywell-user_guide-symbols.tex"
  ;;     left_file_id [cb3fa7b591baf703d41dc2aaa220c9e3b456c4b3]
  ;;       right_name "1553/gds-hardware-bus_1553-iru_honeywell-user_guide-symbols.tex"
  ;;    right_file_id [d1eee768379694a59b2b015dd59a61cf67505182]
  ;;
  ;; optional resolution: {resolved_internal | resolved_user}
  (let ((conflict (make-xmtn-conflicts-conflict)))
    (setf (xmtn-conflicts-conflict-conflict_type conflict) 'content)
    (xmtn-basic-io-check-line "node_type"
      (if (not (string= "file" (cadar value))) (error "expecting \"file\" found %s" (cadar value))))
    (xmtn-basic-io-check-line "ancestor_name" (setf (xmtn-conflicts-conflict-ancestor_name conflict) (cadar value)))
    (xmtn-basic-io-check-line "ancestor_file_id" (setf (xmtn-conflicts-conflict-ancestor_file_id conflict) (cadar value)))
    (xmtn-basic-io-check-line "left_name" (setf (xmtn-conflicts-conflict-left_name conflict) (cadar value)))
    (xmtn-basic-io-check-line "left_file_id" (setf (xmtn-conflicts-conflict-left_file_id conflict) (cadar value)))
    (xmtn-basic-io-check-line "right_name" (setf (xmtn-conflicts-conflict-right_name conflict) (cadar value)))
    (xmtn-basic-io-check-line "right_file_id" (setf (xmtn-conflicts-conflict-right_file_id conflict) (cadar value)))

    ;; look for a resolution
    (case (xmtn-basic-io--peek)
      ((empty eof) nil)
      (t
       (xmtn-basic-io-parse-line
           (progn
             (setq xmtn-conflicts-resolved-count (+ 1 xmtn-conflicts-resolved-count))
             (cond
              ((string= "resolved_internal" symbol)
               (setq xmtn-conflicts-resolved-internal-count (+ 1 xmtn-conflicts-resolved-internal-count))
               (setf (xmtn-conflicts-conflict-left_resolution conflict) (list 'resolved_internal)))

              ((string= "resolved_user" symbol)
               (setf (xmtn-conflicts-conflict-left_resolution conflict) (list 'resolved_user (cadar value))))

              (t
               (error "found %s" symbol)))))))

    (setq xmtn-conflicts-total-count (+ 1 xmtn-conflicts-total-count))

    (xmtn-basic-io-check-empty)

    (ewoc-enter-last xmtn-conflicts-ewoc conflict)))

(defun xmtn-conflicts-parse-duplicate_name ()
  "Fill an ewoc entry with data from duplicate_name conflict stanza."
  ;;      conflict duplicate_name
  ;;     left_type "added file"
  ;;     left_name "checkout_left.sh"
  ;;  left_file_id [ae5fe55181c0307c705d0b05fdc1147fc4afd05c]
  ;;    right_type "added file"
  ;;    right_name "checkout_left.sh"
  ;; right_file_id [355315653eb77ade4804e42a2ef30c89387e1a2d]
  ;;
  ;;   conflict duplicate_name
  ;;  left_type "added directory"
  ;;  left_name "utils"
  ;; right_type "added directory"
  ;; right_name "utils"
  ;;
  ;; optional left and right resolutions:
  ;; resolved_keep{_left | _right}
  ;; resolved_drop{_left | _right}
  ;; resolved_rename{_left | _right} <file>
  ;; resolved_user{_left | _right} <file>
  (let ((conflict (make-xmtn-conflicts-conflict)))
    (setf (xmtn-conflicts-conflict-conflict_type conflict) 'duplicate_name)
    (xmtn-basic-io-check-line "left_type" (setf (xmtn-conflicts-conflict-left_type conflict) (cadar value)))
    (xmtn-basic-io-check-line "left_name" (setf (xmtn-conflicts-conflict-left_name conflict) (cadar value)))
    (xmtn-basic-io-parse-line
        (cond
          ((string= "left_file_id" symbol)
           (setf (xmtn-conflicts-conflict-left_file_id conflict) (cadar value))
           (xmtn-basic-io-check-line "right_type"
             (setf (xmtn-conflicts-conflict-right_type conflict) (cadar value)))
           (xmtn-basic-io-check-line "right_name"
             (setf (xmtn-conflicts-conflict-right_name conflict) (cadar value)))
           (xmtn-basic-io-check-line "right_file_id"
             (setf (xmtn-conflicts-conflict-right_file_id conflict) (cadar value))))

          ((string= "right_type" symbol)
           (setf (xmtn-conflicts-conflict-right_type conflict) (cadar value))
           (xmtn-basic-io-check-line "right_name"
             (setf (xmtn-conflicts-conflict-right_name conflict) (cadar value))))
          (t
           (error "found %s" symbol))))

    ;; look for a left resolution
    (case (xmtn-basic-io--peek)
      ((empty eof) nil)
      (t
       (xmtn-basic-io-parse-line
        (cond
          ((string= "resolved_keep_left" symbol)
           (setf (xmtn-conflicts-conflict-left_resolution conflict) (list 'resolved_keep)))
          ((string= "resolved_drop_left" symbol)
           (setf (xmtn-conflicts-conflict-left_resolution conflict) (list 'resolved_drop)))
          ((string= "resolved_rename_left" symbol)
           (setf (xmtn-conflicts-conflict-left_resolution conflict) (list 'resolved_rename (cadar value))))
          ((string= "resolved_user_left" symbol)
           (setf (xmtn-conflicts-conflict-left_resolution conflict) (list 'resolved_user (cadar value))))
          (t
           (error "left_resolution found %s" symbol))))))

    ;; look for a right resolution
    (case (xmtn-basic-io--peek)
      ((empty eof) nil)
      (t
       (xmtn-basic-io-parse-line
        (cond
          ((string= "resolved_keep_right" symbol)
           (setf (xmtn-conflicts-conflict-right_resolution conflict) (list 'resolved_keep)))
          ((string= "resolved_drop_right" symbol)
           (setf (xmtn-conflicts-conflict-right_resolution conflict) (list 'resolved_drop)))
          ((string= "resolved_rename_right" symbol)
           (setf (xmtn-conflicts-conflict-right_resolution conflict) (list 'resolved_rename (cadar value))))
          ((string= "resolved_user_right" symbol)
           (setf (xmtn-conflicts-conflict-right_resolution conflict) (list 'resolved_user (cadar value))))
          (t
           (error "right_resolution found %s" symbol))))))

    (setq xmtn-conflicts-total-count (+ 1 xmtn-conflicts-total-count))
    (if (and (xmtn-conflicts-conflict-left_resolution conflict)
             (xmtn-conflicts-conflict-right_resolution conflict))
        (setq xmtn-conflicts-resolved-count (+ 1 xmtn-conflicts-resolved-count)))

    (xmtn-basic-io-check-empty)

    (ewoc-enter-last xmtn-conflicts-ewoc conflict)))

(defun xmtn-conflicts-parse-orphaned_node ()
  "Fill an ewoc entry with data from orphaned_node conflict stanza."
  ;;      conflict orphaned_file
  ;;     left_type "deleted directory"
  ;; ancestor_name "patches"
  ;;    right_type "added file"
  ;;    right_name "patches/unrestricted_access.patch"
  ;; right_file_id [597fd36ef183b2b8243d6d2a47fc6c2bf9cb585d]
  ;;
  ;;      conflict orphaned_directory
  ;;     left_type "deleted directory"
  ;; ancestor_name "stuff"
  ;;    right_type "added directory"
  ;;    right_name "stuff/dir1"
  ;;
  ;; or swap left/right
  ;;
  ;; optional resolutions:
  ;; resolved_drop_left
  ;; resolved_rename_left <file>
  (let ((conflict (make-xmtn-conflicts-conflict)))
    (setf (xmtn-conflicts-conflict-conflict_type conflict) 'orphaned_node)
    (xmtn-basic-io-check-line "left_type" (setf (xmtn-conflicts-conflict-left_type conflict) (cadar value)))
    (xmtn-basic-io-parse-line
        (cond
          ((string= "ancestor_name" symbol)
           (setf (xmtn-conflicts-conflict-ancestor_name conflict) (cadar value)))

          ((string= "left_name" symbol)
           (setf (xmtn-conflicts-conflict-left_name conflict) (cadar value))
           (xmtn-basic-io-optional-line "left_file_id"
             (setf (xmtn-conflicts-conflict-left_file_id conflict) (cadar value))))
          (t
           (error "found %s" symbol))))

    (xmtn-basic-io-check-line "right_type" (setf (xmtn-conflicts-conflict-right_type conflict) (cadar value)))
    (xmtn-basic-io-parse-line
        (cond
          ((string= "ancestor_name" symbol)
           (setf (xmtn-conflicts-conflict-ancestor_name conflict) (cadar value)))

          ((string= "right_name" symbol)
           (setf (xmtn-conflicts-conflict-right_name conflict) (cadar value))
           (xmtn-basic-io-optional-line "right_file_id"
             (setf (xmtn-conflicts-conflict-right_file_id conflict) (cadar value))))
          (t
           (error "found %s" symbol))))

    ;; look for a resolution
    (case (xmtn-basic-io--peek)
      ((empty eof) nil)
      (t
       (xmtn-basic-io-parse-line
        (cond
          ((string= "resolved_drop_left" symbol)
           (setf (xmtn-conflicts-conflict-left_resolution conflict) (list 'resolved_drop)))
          ((string= "resolved_rename_left" symbol)
           (setf (xmtn-conflicts-conflict-left_resolution conflict) (list 'resolved_rename (cadar value))))
          (t
           (error "resolution found %s" symbol))))))

    (setq xmtn-conflicts-total-count (+ 1 xmtn-conflicts-total-count))
    (if (xmtn-conflicts-conflict-left_resolution conflict)
        (setq xmtn-conflicts-resolved-count (+ 1 xmtn-conflicts-resolved-count)))

    (xmtn-basic-io-check-empty)

    (ewoc-enter-last xmtn-conflicts-ewoc conflict)))

(defun xmtn-conflicts-parse-conflicts (end)
  "Parse conflict stanzas from point thru END, fill in ewoc."
  ;; first line in stanza indicates type of conflict; dispatch on that
  ;; ewoc-enter-last puts text in the buffer, after `end', preserving point.
  ;; xmtn-basic-io parsing moves point.
  (while (< (point) end)
    (xmtn-basic-io-check-line
     "conflict"
     (cond
      ((and (eq 1 (length value))
            (eq 'symbol (caar value))
            (string= "content" (cadar value)))
       (xmtn-conflicts-parse-content-conflict))

      ((and (eq 1 (length value))
            (eq 'symbol (caar value))
            (string= "duplicate_name" (cadar value)))
       (xmtn-conflicts-parse-duplicate_name))

      ((and (eq 1 (length value))
            (eq 'symbol (caar value))
            (or (string= "orphaned_file" (cadar value))
                (string= "orphaned_directory" (cadar value))))
       (xmtn-conflicts-parse-orphaned_node))

      (t
       (error "unrecognized conflict type %s" value))))))

(defun xmtn-conflicts-set-hf ()
  "Set ewoc header and footer."
  (ewoc-set-hf
   xmtn-conflicts-ewoc
   (concat
    (format "       Left branch : %s\n" xmtn-conflicts-left-branch)
    (format "      Right branch : %s\n" xmtn-conflicts-right-branch)
    (format "   Total conflicts : %d\n" xmtn-conflicts-total-count)
    (format "Resolved conflicts : %d\n" xmtn-conflicts-resolved-count)
    )
   ""))

(defun xmtn-conflicts-read (begin end)
  "Parse region BEGIN END in current buffer as basic-io, fill in ewoc, erase BEGIN END."
  ;; Matches format-alist requirements. We are not currently using
  ;; this in format-alist, but we might someday, and we need these
  ;; params anyway.
  (set-syntax-table xmtn-basic-io--*syntax-table*)
  (goto-char begin)
  (xmtn-conflicts-parse-header)
  (if xmtn-conflicts-ancestor-revision
      (xmtn-conflicts-parse-conflicts (1- end)); off-by-one somewhere.
    ;; else no conflicts
    )
  (let ((inhibit-read-only t)) (delete-region begin (1- end)))
  (xmtn-conflicts-set-hf)
  (set-buffer-modified-p nil)
  (point-max))

(defun xmtn-conflicts-after-insert-file (chars-inserted)
  ;; matches after-insert-file-functions requirements

  ;; `xmtn-conflicts-read' creates ewoc entries, which are
  ;; inserted into the buffer. Since it is parsing the same
  ;; buffer, we need them to be inserted _after_ the text that is
  ;; being parsed. `xmtn-conflicts-mode' creates the ewoc at
  ;; point, and inserts empty header and footer lines.
  (goto-char (point-max))
  (let ((text-end (point)))
    (xmtn-conflicts-mode)
    (xmtn-conflicts-read (point-min) text-end))

  (set-buffer-modified-p nil)
  (point-max)
  (xmtn-conflicts-next nil t))

(defun xmtn-conflicts-write-header (ewoc-buffer)
  "Write EWOC-BUFFER header info in basic-io format to current buffer."
  (xmtn-basic-io-write-id "left" (with-current-buffer ewoc-buffer xmtn-conflicts-left-revision))
  (xmtn-basic-io-write-id "right" (with-current-buffer ewoc-buffer xmtn-conflicts-right-revision))
  (if xmtn-conflicts-ancestor-revision
      (xmtn-basic-io-write-id "ancestor" (with-current-buffer ewoc-buffer xmtn-conflicts-ancestor-revision)))
  )

(defun xmtn-conflicts-write-content (conflict)
  "Write CONFLICT (a content conflict) in basic-io format to current buffer."
  (insert ?\n)
  (xmtn-basic-io-write-sym "conflict" "content")
  (xmtn-basic-io-write-str "node_type" "file")
  (xmtn-basic-io-write-str "ancestor_name" (xmtn-conflicts-conflict-ancestor_name conflict))
  (xmtn-basic-io-write-id "ancestor_file_id" (xmtn-conflicts-conflict-ancestor_file_id conflict))
  (xmtn-basic-io-write-str "left_name" (xmtn-conflicts-conflict-left_name conflict))
  (xmtn-basic-io-write-id "left_file_id" (xmtn-conflicts-conflict-left_file_id conflict))
  (xmtn-basic-io-write-str "right_name" (xmtn-conflicts-conflict-right_name conflict))
  (xmtn-basic-io-write-id "right_file_id" (xmtn-conflicts-conflict-right_file_id conflict))

  (if (xmtn-conflicts-conflict-left_resolution conflict)
      (progn
        (setq xmtn-conflicts-resolved-count (+ 1 xmtn-conflicts-resolved-count))
        (ecase (car (xmtn-conflicts-conflict-left_resolution conflict))
          (resolved_internal
           (setq xmtn-conflicts-resolved-internal-count (+ 1 xmtn-conflicts-resolved-internal-count))
           (insert "resolved_internal \n"))

          (resolved_keep
           (insert "resolved_keep_left \n"))

          (resolved_user
           (xmtn-basic-io-write-str "resolved_user" (cadr (xmtn-conflicts-conflict-left_resolution conflict))))
          ))))

(defun xmtn-conflicts-write-duplicate_name (conflict)
  "Write CONFLICT (a duplicate_name conflict) in basic-io format to current buffer."
  (insert ?\n)
  (xmtn-basic-io-write-sym "conflict" "duplicate_name")
  (xmtn-basic-io-write-str "left_type" (xmtn-conflicts-conflict-left_type conflict))
  (xmtn-basic-io-write-str "left_name" (xmtn-conflicts-conflict-left_name conflict))
  (if (xmtn-conflicts-conflict-left_file_id conflict)
      (xmtn-basic-io-write-id "left_file_id" (xmtn-conflicts-conflict-left_file_id conflict)))
  (xmtn-basic-io-write-str "right_type" (xmtn-conflicts-conflict-right_type conflict))
  (xmtn-basic-io-write-str "right_name" (xmtn-conflicts-conflict-right_name conflict))
  (if (xmtn-conflicts-conflict-right_file_id conflict)
      (xmtn-basic-io-write-id "right_file_id" (xmtn-conflicts-conflict-right_file_id conflict)))

  (if (xmtn-conflicts-conflict-left_resolution conflict)
      (ecase (car (xmtn-conflicts-conflict-left_resolution conflict))
        (resolved_keep
         (insert "resolved_keep_left \n"))
        (resolved_drop
         (insert "resolved_drop_left \n"))
        (resolved_rename
         (xmtn-basic-io-write-str
          "resolved_rename_left"
          (file-relative-name (cadr (xmtn-conflicts-conflict-left_resolution conflict)))))
        (resolved_user
         (xmtn-basic-io-write-str
          "resolved_user_left"
          (file-relative-name (cadr (xmtn-conflicts-conflict-left_resolution conflict)))))
        ))

  (if (xmtn-conflicts-conflict-right_resolution conflict)
      (ecase (car (xmtn-conflicts-conflict-right_resolution conflict))
        (resolved_keep
         (insert "resolved_keep_right \n"))
        (resolved_drop
         (insert "resolved_drop_right \n"))
        (resolved_rename
         (xmtn-basic-io-write-str
          "resolved_rename_right"
          (file-relative-name (cadr (xmtn-conflicts-conflict-right_resolution conflict)))))
        (resolved_user
         (xmtn-basic-io-write-str
          "resolved_user_right"
          (file-relative-name (cadr (xmtn-conflicts-conflict-right_resolution conflict)))))
        ))

  (if (and (xmtn-conflicts-conflict-left_resolution conflict)
           (xmtn-conflicts-conflict-right_resolution conflict))
      (setq xmtn-conflicts-resolved-count (+ 1 xmtn-conflicts-resolved-count)))
  )

(defun xmtn-conflicts-write-orphaned_node (conflict)
  "Write CONFLICT (an orphaned_node conflict) in basic-io format to current buffer."
  (insert ?\n)
  (cond
   ((string= "added directory" (xmtn-conflicts-conflict-left_type conflict))
    (xmtn-basic-io-write-sym "conflict" "orphaned_directory")
    (xmtn-basic-io-write-str "left_type" "added directory")
    (xmtn-basic-io-write-str "left_name" (xmtn-conflicts-conflict-left_name conflict))
    (xmtn-basic-io-write-str "right_type" (xmtn-conflicts-conflict-right_type conflict))
    (xmtn-basic-io-write-str "ancestor_name" (xmtn-conflicts-conflict-ancestor_name conflict)))

   ((string= "added file" (xmtn-conflicts-conflict-left_type conflict))
    (xmtn-basic-io-write-sym "conflict" "orphaned_file")
    (xmtn-basic-io-write-str "left_type" "added file")
    (xmtn-basic-io-write-str "left_name" (xmtn-conflicts-conflict-left_name conflict))
    (xmtn-basic-io-write-id "left_file_id" (xmtn-conflicts-conflict-left_file_id conflict))
    (xmtn-basic-io-write-str "right_type" (xmtn-conflicts-conflict-right_type conflict))
    (xmtn-basic-io-write-str "ancestor_name" (xmtn-conflicts-conflict-ancestor_name conflict)))

   ((string= "added directory" (xmtn-conflicts-conflict-right_type conflict))
    (xmtn-basic-io-write-sym "conflict" "orphaned_directory")
    (xmtn-basic-io-write-str "left_type" (xmtn-conflicts-conflict-left_type conflict))
    (xmtn-basic-io-write-str "ancestor_name" (xmtn-conflicts-conflict-ancestor_name conflict))
    (xmtn-basic-io-write-str "right_type" "added directory")
    (xmtn-basic-io-write-str "right_name" (xmtn-conflicts-conflict-right_name conflict)))

   ((string= "added file" (xmtn-conflicts-conflict-right_type conflict))
    (xmtn-basic-io-write-sym "conflict" "orphaned_file")
    (xmtn-basic-io-write-str "left_type" (xmtn-conflicts-conflict-left_type conflict))
    (xmtn-basic-io-write-str "ancestor_name" (xmtn-conflicts-conflict-ancestor_name conflict))
    (xmtn-basic-io-write-str "right_type" (xmtn-conflicts-conflict-right_type conflict))
    (xmtn-basic-io-write-str "right_name" (xmtn-conflicts-conflict-right_name conflict))
    (xmtn-basic-io-write-id "right_file_id" (xmtn-conflicts-conflict-right_file_id conflict)))
   )

  (if (xmtn-conflicts-conflict-left_resolution conflict)
      (progn
        (setq xmtn-conflicts-resolved-count (+ 1 xmtn-conflicts-resolved-count))
        (ecase (car (xmtn-conflicts-conflict-left_resolution conflict))
          (resolved_drop
           (insert "resolved_drop_left \n"))

          (resolved_rename
           (xmtn-basic-io-write-str "resolved_rename_left" (cadr (xmtn-conflicts-conflict-left_resolution conflict))))
          ))))

(defun xmtn-conflicts-write-conflicts (ewoc)
  "Write EWOC elements in basic-io format to xmtn-conflicts-output-buffer."
  (setq xmtn-conflicts-resolved-count 0)
  (setq xmtn-conflicts-resolved-internal-count 0)
  (ewoc-map
   (lambda (conflict)
     (with-current-buffer xmtn-conflicts-output-buffer
       (ecase (xmtn-conflicts-conflict-conflict_type conflict)
         (content
          (xmtn-conflicts-write-content conflict))
         (duplicate_name
          (xmtn-conflicts-write-duplicate_name conflict))
         (orphaned_node
          (xmtn-conflicts-write-orphaned_node conflict))
         )))
   ewoc))

(defun xmtn-conflicts-save (begin end ewoc-buffer)
  "Replace region BEGIN END with EWOC-BUFFER ewoc in basic-io format."
  (delete-region begin end)
  (xmtn-conflicts-write-header ewoc-buffer)
  ;; ewoc-map sets current-buffer to ewoc-buffer, so we need a
  ;; reference to the current buffer.
  (let ((xmtn-conflicts-output-buffer (current-buffer))
        (ewoc (with-current-buffer ewoc-buffer xmtn-conflicts-ewoc)))
    (xmtn-conflicts-write-conflicts ewoc)
    (with-current-buffer ewoc-buffer (xmtn-conflicts-set-hf))
    ))

;; Arrange for xmtn-conflicts-save to be called by save-buffer. We do
;; not automatically convert in insert-file-contents, because we don't
;; want to convert _all_ conflict files (consider the monotone test
;; suite!). Instead, we call xmtn-conflicts-read explicitly from
;; xmtn-conflicts-review, and set after-insert-file-functions to a
;; buffer-local value in xmtn-conflicts-mode.
(add-to-list 'format-alist
             '(xmtn-conflicts-format
               "Save conflicts in basic-io format."
               nil
               nil
               xmtn-conflicts-save
               t
               nil
               nil))

(defun xmtn-conflicts-update-counts ()
  "Update resolved counts."
  (setq xmtn-conflicts-resolved-count 0)
  (setq xmtn-conflicts-resolved-internal-count 0)

  (ewoc-map
   (lambda (conflict)
     (ecase (xmtn-conflicts-conflict-conflict_type conflict)
       (content
        (if (xmtn-conflicts-conflict-left_resolution conflict)
            (progn
              (setq xmtn-conflicts-resolved-count (+ 1 xmtn-conflicts-resolved-count))
              (if (eq 'resolved_internal (car (xmtn-conflicts-conflict-left_resolution conflict)))
                  (setq xmtn-conflicts-resolved-internal-count (+ 1 xmtn-conflicts-resolved-internal-count))))))

       (duplicate_name
        (if (and (xmtn-conflicts-conflict-left_resolution conflict)
                 (xmtn-conflicts-conflict-right_resolution conflict))
            (setq xmtn-conflicts-resolved-count (+ 1 xmtn-conflicts-resolved-count))))

       (orphaned_node
        (if (xmtn-conflicts-conflict-left_resolution conflict)
            (setq xmtn-conflicts-resolved-count (+ 1 xmtn-conflicts-resolved-count))))

       ))
   xmtn-conflicts-ewoc))

(dvc-make-ewoc-next xmtn-conflicts-next xmtn-conflicts-ewoc)
(dvc-make-ewoc-prev xmtn-conflicts-prev xmtn-conflicts-ewoc)

(defun xmtn-conflicts-resolvedp (elem)
  "Return non-nil if ELEM contains a complete conflict resolution."
  (let ((conflict (ewoc-data elem)))
    (ecase (xmtn-conflicts-conflict-conflict_type conflict)
      ((content orphaned_node)
       (xmtn-conflicts-conflict-left_resolution conflict))
      (duplicate_name
       (and (xmtn-conflicts-conflict-left_resolution conflict)
            (xmtn-conflicts-conflict-right_resolution conflict)))
      )))

(defun xmtn-conflicts-next-unresolved ()
  "Move to next unresolved element."
  (interactive)
  (xmtn-conflicts-next 'xmtn-conflicts-resolvedp))

(defun xmtn-conflicts-prev-unresolved ()
  "Move to prev unresolved element."
  (interactive)
  (xmtn-conflicts-prev 'xmtn-conflicts-resolvedp))

(defun xmtn-conflicts-clear-resolution()
  "Remove resolution for current conflict."
  (interactive)
  (let* ((elem (ewoc-locate xmtn-conflicts-ewoc))
         (conflict (ewoc-data elem)))
    (setf (xmtn-conflicts-conflict-left_resolution conflict) nil)
    (setf (xmtn-conflicts-conflict-right_resolution conflict) nil)
    (ewoc-invalidate xmtn-conflicts-ewoc elem)))

(defun xmtn-conflicts-resolve-conflict-post-ediff ()
  "Stuff to do when ediff quits."
  (remove-hook 'ediff-quit-merge-hook 'xmtn-conflicts-resolve-conflict-post-ediff)
  (add-hook 'ediff-quit-merge-hook 'ediff-maybe-save-and-delete-merge)

  (ediff-dispose-of-variant-according-to-user ediff-buffer-A 'A nil nil)
  (ediff-dispose-of-variant-according-to-user ediff-buffer-B 'B nil nil)
  (ediff-dispose-of-variant-according-to-user ediff-ancestor-buffer 'Ancestor nil nil)
  (save-excursion
    (set-buffer ediff-buffer-C)
    (save-buffer))
  (ediff-kill-buffer-carefully ediff-buffer-C)

  (let ((control-buffer ediff-control-buffer))
    (pop-to-buffer xmtn-conflicts-current-conflict-buffer)
    (setq xmtn-conflicts-current-conflict-buffer nil)
    (let ((current     (nth 0 xmtn-conflicts-ediff-quit-info))
          (result-file (nth 1 xmtn-conflicts-ediff-quit-info))
          (window-config (nth 2 xmtn-conflicts-ediff-quit-info)))
      (let ((conflict (ewoc-data current)))
        (ecase (xmtn-conflicts-conflict-conflict_type conflict)
          (content
           (setf (xmtn-conflicts-conflict-left_resolution conflict) (list 'resolved_user result-file)))
          (duplicate_name
           (ecase (nth 3 xmtn-conflicts-ediff-quit-info); side
             ('left
              (setf (xmtn-conflicts-conflict-left_resolution conflict) (list 'resolved_user result-file)))
             ('right
              (setf (xmtn-conflicts-conflict-right_resolution conflict) (list 'resolved_user result-file)))
             ))
           ;; can't resolve orphaned_node by ediff
          ))
      (ewoc-invalidate xmtn-conflicts-ewoc current)
      (set-window-configuration window-config)
      (set-buffer control-buffer))))

(defun xmtn-conflicts-get-file (file-id dir file-name)
  "Get contents of FILE-ID into DIR/FILE-NAME. Return full file name."
  (let ((file (concat (file-name-as-directory dir) file-name)))
    (setq dir (file-name-directory file))
    (unless (file-exists-p dir)
      (make-directory dir t))
    (xmtn--get-file-by-id default-directory file-id file)
    file))

(defun xmtn-conflicts-resolve-ediff (side)
  "Resolve the current conflict via ediff SIDE."
  (interactive)
  (if xmtn-conflicts-current-conflict-buffer
      (error "another conflict resolution is already in progress."))

  (let* ((elem (ewoc-locate xmtn-conflicts-ewoc))
         (conflict (ewoc-data elem))
         (type (xmtn-conflicts-conflict-conflict_type conflict)))

    (if (not (xmtn-conflicts-conflict-left_file_id conflict))
        (error "can't ediff directories from here"))

    ;; Get the ancestor, left, right into files with nice names, so
    ;; uniquify gives the buffers nice names. Store the result in
    ;; _MTN/*, so a later 'merge --resolve-conflicts-file' can find it.
    ;;
    ;; duplicate_name conflicts have no ancestor.
    (let ((file-ancestor (and (xmtn-conflicts-conflict-ancestor_file_id conflict)
                              (xmtn-conflicts-get-file (xmtn-conflicts-conflict-ancestor_file_id conflict)
                                                       "_MTN/resolutions/ancestor"
                                                       (xmtn-conflicts-conflict-ancestor_name conflict))))
          (file-left (xmtn-conflicts-get-file (xmtn-conflicts-conflict-left_file_id conflict)
                                              xmtn-conflicts-left-root
                                              (xmtn-conflicts-conflict-left_name conflict)))
          (file-right (xmtn-conflicts-get-file (xmtn-conflicts-conflict-right_file_id conflict)
                                               xmtn-conflicts-right-root
                                               (xmtn-conflicts-conflict-right_name conflict)))

          (result-file (concat "_MTN/resolutions/result/" (xmtn-conflicts-conflict-right_name conflict))) )

      (unless (file-exists-p (file-name-directory result-file))
        (make-directory (file-name-directory result-file) t))

      (remove-hook 'ediff-quit-merge-hook 'ediff-maybe-save-and-delete-merge)
      (add-hook 'ediff-quit-merge-hook 'xmtn-conflicts-resolve-conflict-post-ediff)

      ;; ediff leaves the merge buffer active;
      ;; xmtn-conflicts-resolve-conflict-post-ediff needs to find the
      ;; conflict buffer.
      (setq xmtn-conflicts-current-conflict-buffer (current-buffer))
      (setq xmtn-conflicts-ediff-quit-info
            (list elem result-file (current-window-configuration) side))

      (if file-ancestor
          (ediff-merge-files-with-ancestor file-left file-right file-ancestor nil result-file)
        (ediff-merge-files file-left file-right nil result-file))
      )))

(defun xmtn-conflicts-resolve-keep_left ()
  "Resolve the current conflict by keep_left."
  (interactive)
  (let* ((elem (ewoc-locate xmtn-conflicts-ewoc))
         (conflict (ewoc-data elem)))
    (setf (xmtn-conflicts-conflict-left_resolution conflict) (list 'resolved_keep))
    (ewoc-invalidate xmtn-conflicts-ewoc elem)))

(defun xmtn-conflicts-resolve-keep_right ()
  "Resolve the current conflict by keep_right."
  (interactive)
  (let* ((elem (ewoc-locate xmtn-conflicts-ewoc))
         (conflict (ewoc-data elem)))
    (setf (xmtn-conflicts-conflict-right_resolution conflict) (list 'resolved_keep))
    (ewoc-invalidate xmtn-conflicts-ewoc elem)))

(defun xmtn-conflicts-resolve-drop_left ()
  "Resolve the current conflict by drop_left."
  (interactive)
  (let* ((elem (ewoc-locate xmtn-conflicts-ewoc))
         (conflict (ewoc-data elem)))
    (setf (xmtn-conflicts-conflict-left_resolution conflict) (list 'resolved_drop))
    (ewoc-invalidate xmtn-conflicts-ewoc elem)))

(defun xmtn-conflicts-resolve-drop_right ()
  "Resolve the current conflict by drop_right."
  (interactive)
  (let* ((elem (ewoc-locate xmtn-conflicts-ewoc))
         (conflict (ewoc-data elem)))
    (setf (xmtn-conflicts-conflict-right_resolution conflict) (list 'resolved_drop))
    (ewoc-invalidate xmtn-conflicts-ewoc elem)))

(defun xmtn-conflicts-resolve-user (resolve-side default-side)
  "Resolve the current conflict by user_RESOLVE-SIDE. Default to file from DEFAULT-SIDE."
  (interactive)
  (let* ((elem (ewoc-locate xmtn-conflicts-ewoc))
         (conflict (ewoc-data elem))
         (result-file
          (expand-file-name
           (read-file-name "resolution file: "
                           (ecase default-side
                             (left (file-name-as-directory xmtn-conflicts-left-work))
                             (right (file-name-as-directory xmtn-conflicts-right-work)))
                           nil t
                           (ecase default-side
                             (left (xmtn-conflicts-conflict-left_name conflict))
                             (right (xmtn-conflicts-conflict-right_name conflict)))))))
    (ecase resolve-side
      (left
       (setf (xmtn-conflicts-conflict-left_resolution conflict) (list 'resolved_user result-file)))
      (right
        (setf (xmtn-conflicts-conflict-right_resolution conflict) (list 'resolved_user result-file)))
      )
    (ewoc-invalidate xmtn-conflicts-ewoc elem)))

(defun xmtn-conflicts-resolve-rename (side)
  "Resolve the current conflict by rename_SIDE."
  (interactive)
  ;; Right is the target workspace in a propagate, and also the current
  ;; workspace in a merge. So default to right_name.
  (let* ((elem (ewoc-locate xmtn-conflicts-ewoc))
         (conflict (ewoc-data elem))
         (result-file
          (file-relative-name
           (read-file-name "rename file: " "" nil nil
                           (concat "/" (xmtn-conflicts-conflict-right_name conflict))))))
      (ecase side
        ('left
         (setf (xmtn-conflicts-conflict-left_resolution conflict) (list 'resolved_rename result-file)))
        ('right
         (setf (xmtn-conflicts-conflict-right_resolution conflict) (list 'resolved_rename result-file)))
        )
      (ewoc-invalidate xmtn-conflicts-ewoc elem)))

(defun xmtn-conflicts-resolve-user_leftp ()
  "Non-nil if user_left resolution is appropriate for current conflict."
  (let* ((conflict (ewoc-data (ewoc-locate xmtn-conflicts-ewoc)))
         (type (xmtn-conflicts-conflict-conflict_type conflict)))

    (and (not (xmtn-conflicts-conflict-left_resolution conflict))
         (or (equal type 'content)
             (and (equal type 'duplicate_name)
                  ;; if no file_id, it's a directory
                  (xmtn-conflicts-conflict-left_file_id conflict))) )))

(defun xmtn-conflicts-resolve-user_rightp ()
  "Non-nil if user_right resolution is appropriate for current conflict."
  (let* ((conflict (ewoc-data (ewoc-locate xmtn-conflicts-ewoc)))
         (type (xmtn-conflicts-conflict-conflict_type conflict)))

    ;; duplicate_name is the only conflict type that needs a right resolution
    (and (xmtn-conflicts-conflict-left_resolution conflict)
         (not (xmtn-conflicts-conflict-right_resolution conflict))
         (equal type 'duplicate_name)
         ;; if no file_id, it's a directory
         (xmtn-conflicts-conflict-right_file_id conflict)
         ;; user_right doesn't change name, so left resolution must change name or drop
         (let ((left-res (car (xmtn-conflicts-conflict-left_resolution conflict))))
           (member left-res '(resolved_drop resolved_rename))))))

(defun xmtn-conflicts-resolve-keep_leftp ()
  "Non-nil if keep_left resolution is appropriate for current conflict."
  (let* ((conflict (ewoc-data (ewoc-locate xmtn-conflicts-ewoc)))
         (type (xmtn-conflicts-conflict-conflict_type conflict)))

    (and (not (xmtn-conflicts-conflict-left_resolution conflict))
         (equal type 'duplicate_name))))

(defun xmtn-conflicts-resolve-keep_rightp ()
  "Non-nil if keep_right resolution is appropriate for current conflict."
  (let* ((conflict (ewoc-data (ewoc-locate xmtn-conflicts-ewoc)))
         (type (xmtn-conflicts-conflict-conflict_type conflict)))

    ;; duplicate_name is the only conflict type that needs a right resolution
    (and (xmtn-conflicts-conflict-left_resolution conflict)
         (not (xmtn-conflicts-conflict-right_resolution conflict))
         (equal type 'duplicate_name)
         (let ((left-res (car (xmtn-conflicts-conflict-left_resolution conflict))))
           (member left-res '(resolved_drop resolved_rename))))))

(defun xmtn-conflicts-resolve-rename_leftp ()
  "Non-nil if rename_left resolution is appropriate for current conflict."
  (let* ((conflict (ewoc-data (ewoc-locate xmtn-conflicts-ewoc)))
         (type (xmtn-conflicts-conflict-conflict_type conflict)))

    (and (not (xmtn-conflicts-conflict-left_resolution conflict))
         (member type '(duplicate_name orphaned_node)))))

(defun xmtn-conflicts-resolve-rename_rightp ()
  "Non-nil if rename_right resolution is appropriate for current conflict."
  (let* ((conflict (ewoc-data (ewoc-locate xmtn-conflicts-ewoc)))
         (type (xmtn-conflicts-conflict-conflict_type conflict)))

    ;; duplicate_name is the only conflict type that needs a right resolution
    (and (xmtn-conflicts-conflict-left_resolution conflict)
         (not (xmtn-conflicts-conflict-right_resolution conflict))
         (equal type 'duplicate_name))))

(defun xmtn-conflicts-resolve-drop_leftp ()
  "Non-nil if drop_left resolution is appropriate for the current conflict."
  (let* ((conflict (ewoc-data (ewoc-locate xmtn-conflicts-ewoc)))
         (type (xmtn-conflicts-conflict-conflict_type conflict)))

    (and (not (xmtn-conflicts-conflict-left_resolution conflict))
         (or (and (equal type 'duplicate_name)
                  ;; if no file_id, it's a directory; can't drop if not empty
                  (xmtn-conflicts-conflict-left_file_id conflict))
             (and (equal type 'orphaned_node)
                  ;; if no left or right file_id, it's a directory; can't drop if not empty
                  (or (xmtn-conflicts-conflict-left_file_id conflict)
                      (xmtn-conflicts-conflict-right_file_id conflict)
                      ))))))

(defun xmtn-conflicts-resolve-drop_rightp ()
  "Non-nil if drop_right resolution is appropriate for the current conflict."
  (let* ((conflict (ewoc-data (ewoc-locate xmtn-conflicts-ewoc)))
         (type (xmtn-conflicts-conflict-conflict_type conflict)))

    ;; duplicate_name is the only conflict type that needs a right resolution
    (and (xmtn-conflicts-conflict-left_resolution conflict)
         (not (xmtn-conflicts-conflict-right_resolution conflict))
         (equal type 'duplicate_name)
         ;; if no file_id, it's a directory; can't drop if not empty
         (xmtn-conflicts-conflict-right_file_id conflict))))

(defvar xmtn-conflicts-resolve-map
  (let ((map (make-sparse-keymap "resolution")))
    (define-key map [?c]  '(menu-item "c) clear resolution"
                                      xmtn-conflicts-clear-resolution))

    ;; Don't need 'left' or 'right' in menu, since only one is
    ;; visible; then this works better for single file conflicts.

    (define-key map [?b]  '(menu-item "b) drop"
                                      xmtn-conflicts-resolve-drop_right
                                      :visible (xmtn-conflicts-resolve-drop_rightp)))
    (define-key map [?a]  '(menu-item "a) rename"
                                      (lambda ()
                                        (interactive)
                                        (xmtn-conflicts-resolve-rename 'right))
                                      :visible (xmtn-conflicts-resolve-rename_rightp)))
    (define-key map [?9]  '(menu-item "9) right file"
                                      (lambda ()
                                        (interactive)
                                        (xmtn-conflicts-resolve-user 'right 'right))
                                      :visible (xmtn-conflicts-resolve-user_rightp)))
    (define-key map [?8]  '(menu-item "8) left file"
                                      (lambda ()
                                        (interactive)
                                        (xmtn-conflicts-resolve-user 'right 'left))
                                      :visible (xmtn-conflicts-resolve-user_rightp)))
    (define-key map [?7]  '(menu-item "7) keep"
                                      xmtn-conflicts-resolve-keep_right
                                      :visible (xmtn-conflicts-resolve-keep_rightp)))
    (define-key map [?6]  '(menu-item "6) ediff"
                                      (lambda ()
                                        (interactive)
                                        (xmtn-conflicts-resolve-ediff 'right))
                                      :visible (xmtn-conflicts-resolve-user_rightp)))

    (define-key map [?5]  '(menu-item "5) right file"
                                      (lambda ()
                                        (interactive)
                                        (xmtn-conflicts-resolve-user 'left 'right))
                                      :visible (xmtn-conflicts-resolve-user_leftp)))
    (define-key map [?4]  '(menu-item "4) left file"
                                      (lambda ()
                                        (interactive)
                                        (xmtn-conflicts-resolve-user 'left 'left))
                                      :visible (xmtn-conflicts-resolve-user_leftp)))
    (define-key map [?3]  '(menu-item "3) drop"
                                      xmtn-conflicts-resolve-drop_left
                                      :visible (xmtn-conflicts-resolve-drop_leftp)))
    (define-key map [?2]  '(menu-item "2) rename"
                                      (lambda ()
                                        (interactive)
                                        (xmtn-conflicts-resolve-rename 'left))
                                      :visible (xmtn-conflicts-resolve-rename_leftp)))
    (define-key map [?1]  '(menu-item "1) keep"
                                      xmtn-conflicts-resolve-keep_left
                                      :visible (xmtn-conflicts-resolve-keep_leftp)))
    (define-key map [?0]  '(menu-item "0) ediff"
                                      (lambda ()
                                        (interactive)
                                        (xmtn-conflicts-resolve-ediff 'left))
                                      :visible (xmtn-conflicts-resolve-user_leftp)))
    map)
  "Keyboard menu keymap used to resolve conflicts.")

(defun xmtn-conflicts-current-conflict ()
  "Return the conflict (an xmtn-conflicts-root class struct) for
the ewoc element at point.  Throws an error if point is not on a
conflict."
  (let ((ewoc-entry (ewoc-locate xmtn-conflicts-ewoc)))
    (if (not ewoc-entry)
        ;; ewoc is empty
        (error "not on a conflict"))
    (ewoc-data ewoc-entry)))

(defun xmtn-conflicts-add-log-entry (&optional other-frame)
  "Add an entry in the current log-edit buffer for the current file.
If OTHER-FRAME (default prefix) xor `dvc-log-edit-other-frame' is
non-nil, show log-edit buffer in other frame."
  (interactive "P")
  (let ((conflict (xmtn-conflicts-current-conflict)))
    (dvc-log-edit other-frame t)
    (undo-boundary)
    (goto-char (point-max))
    (newline 2)
    (insert "* ")
    (insert (xmtn-conflicts-conflict-right_name conflict))
    (insert ": ")
    ))

(defun xmtn-conflicts-do-propagate (&optional cached-branch)
  "Perform propagate on revisions in current conflict buffer."
  (interactive)
  (save-some-buffers t); log buffer
  ;; save-some-buffers does not save the conflicts buffer, which is the current buffer
  (save-buffer)
  (xmtn-propagate-from xmtn-conflicts-left-branch cached-branch))

(defun xmtn-conflicts-do-merge ()
  "Perform merge on revisions in current conflict buffer."
  (interactive)
  (save-some-buffers t); log buffer
  ;; save-some-buffers does not save the conflicts buffer, which is the current buffer
  (save-buffer)
  (xmtn-dvc-merge-1 default-directory nil))

(defun xmtn-conflicts-ediff-resolution-ws ()
  "Ediff current resolution file against workspace."
  (interactive)
  (let* ((elem (ewoc-locate xmtn-conflicts-ewoc))
         (conflict (ewoc-data elem)))
    (if (and (member (xmtn-conflicts-conflict-conflict_type conflict)
                     '(content orphaned_node))
             (xmtn-conflicts-conflict-left_resolution conflict))
        (ediff (cadr (xmtn-conflicts-conflict-left_resolution conflict))
               ;; propagate target is right
               (xmtn-conflicts-conflict-right_name conflict)))))

(defvar xmtn-conflicts-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [?C]  (lambda () (interactive) (xmtn-conflicts-clean xmtn-conflicts-right-work)))
    (define-key map [?c]  'xmtn-conflicts-clear-resolution)
    (define-key map [?e]  'xmtn-conflicts-ediff-resolution-ws)
    (define-key map [?n]  'xmtn-conflicts-next)
    (define-key map [?N]  'xmtn-conflicts-next-unresolved)
    (define-key map [?p]  'xmtn-conflicts-prev)
    (define-key map [?P]  'xmtn-conflicts-prev-unresolved)
    (define-key map [?q]  'dvc-buffer-quit)
    (define-key map [?r]  xmtn-conflicts-resolve-map)
    (define-key map [?t]  'xmtn-conflicts-add-log-entry)
    (define-key map "\M-d" xmtn-conflicts-resolve-map)
    (define-key map "MM" 'xmtn-conflicts-do-merge)
    (define-key map "MP" 'xmtn-conflicts-do-propagate)
    (define-key map "MU" 'dvc-update)
    map)
  "Keymap used in `xmtn-conflicts-mode'.")

(easy-menu-define xmtn-conflicts-mode-menu xmtn-conflicts-mode-map
  "`xmtn-conflicts' menu"
  `("Mtn-conflicts"
    ["Clear resolution"     xmtn-conflicts-clear-resolution t]
    ["Ediff resolution to ws" xmtn-conflicts-ediff-resolution-ws t]
    ["Propagate"            xmtn-conflicts-do-propagate t]
    ["Merge"                xmtn-conflicts-do-merge t]
    ["Update"               dvc-update t]
    ["Clean"                xmtn-conflicts-clean t]
    ))

;; derive from nil causes no keymap to be used, but still have self-insert keys
;; derive from fundamental-mode causes self-insert keys
(define-derived-mode xmtn-conflicts-mode fundamental-mode "xmtn-conflicts"
  "Major mode to specify conflict resolutions."
  (setq dvc-buffer-current-active-dvc 'xmtn)
  (setq buffer-read-only nil)
  (setq xmtn-conflicts-ewoc (ewoc-create 'xmtn-conflicts-printer))
  (setq dvc-buffer-refresh-function nil)
  (add-to-list 'buffer-file-format 'xmtn-conflicts-format)

  ;; Arrange for `revert-buffer' to do the right thing
  (set (make-local-variable 'after-insert-file-functions) '(xmtn-conflicts-after-insert-file))

  ;; don't do normal clean up stuff
  (set (make-local-variable 'before-save-hook) nil)
  (set (make-local-variable 'write-file-functions) nil)

  (dvc-install-buffer-menu)
  (setq buffer-read-only t)
  (buffer-disable-undo)
  (set-buffer-modified-p nil))

(dvc-add-uniquify-directory-mode 'xmtn-conflicts-mode)

(defconst xmtn-conflicts-opts-file "_MTN/dvc-conflicts-opts")

(defun xmtn-conflicts-save-opts (left-work right-work &optional left-branch right-branch)
  "Store LEFT-WORK, RIGHT-WORK in `xmtn-conflicts-opts-file', for
retrieval by `xmtn-conflicts-load-opts'."
  (let ((xmtn-conflicts-left-work left-work)
        (xmtn-conflicts-right-work right-work)
        (xmtn-conflicts-left-branch (or left-branch
                                        (xmtn--tree-default-branch left-work)))
        (xmtn-conflicts-right-branch (or right-branch
                                         (xmtn--tree-default-branch right-work))))

  (dvc-save-state (list 'xmtn-conflicts-left-work
                        'xmtn-conflicts-left-branch
                        'xmtn-conflicts-right-work
                        'xmtn-conflicts-right-branch)
                  (concat (file-name-as-directory right-work) xmtn-conflicts-opts-file))
  ))

(defun xmtn-conflicts-load-opts ()
  "Load options saved by
`xmtn-conflicts-save-opts'. `default-directory' must be workspace
root where options file is stored."
  (let ((opts-file (concat default-directory xmtn-conflicts-opts-file)))
    (if (file-exists-p opts-file)
        (load opts-file)
      ;; When reviewing conflicts after a merge is complete, the options file is not present
      (message "%s options file not found" opts-file))))

(defun xmtn-conflicts-1 (left-work left-rev right-work right-rev)
  "List conflicts between LEFT-REV and RIGHT-REV
revisions (monotone revision specs; if nil, defaults to heads of
respective workspace branches) in LEFT-WORK and RIGHT-WORK
workspaces (strings).  Allow specifying resolutions, propagating
to right.  Stores conflict file in RIGHT-WORK/_MTN."
  (let ((default-directory right-work))
    (xmtn-conflicts-save-opts left-work right-work)
    (dvc-run-dvc-async
     'xmtn
     (list "conflicts" "store" left-rev right-rev)
     :finished (lambda (output error status arguments)
                 (xmtn-conflicts-review default-directory))

     :error (lambda (output error status arguments)
              (pop-to-buffer error))
     )))

(defun xmtn-check-workspace-for-propagate (work cached-branch)
  "Check that workspace WORK is ready for propagate.
It must be merged, and should be at the head revision, and have no local changes."
  (let* ((default-directory work)
         (heads (xmtn--heads default-directory cached-branch))
         (base (xmtn--get-base-revision-hash-id-or-null default-directory)))

    (message "checking %s for multiple heads, base not head" work)

    (if (> 1 (length heads))
        (error "%s has multiple heads; can't propagate" work))

    (if (not (string= base (nth 0 heads)))
            (error "Aborting due to %s not at head" work))

    ;; check for local changes
    (message "checking %s for local changes" work)

    (dvc-run-dvc-sync
     'xmtn
     (list "status")
     :finished (lambda (output error status arguments)
                 ;; we don't get an error status for not up-to-date,
                 ;; so parse the output.
                 ;; FIXME: add option to automate inventory to just return status; can return on first change
                 ;; FIXME: 'patch' may be internationalized.
                 (set-buffer output)
                 (goto-char (point-min))
                 (if (search-forward "patch" (point-max) t)
                     (if (not (yes-or-no-p (format "%s has local changes; really show conflicts? " work)))
                         (error "aborting due to local changes"))))

     :error (lambda (output error status arguments)
              (pop-to-buffer error))))

  )

(defun xmtn-check-propagate-needed (left-work right-work)
  "Throw error unless branch in workspace LEFT-WORK needs to be propagated to RIGHT-WORK."
  ;; We assume xmtn-check-workspace-for-propagate has already been run
  ;; on left-work, right-work, so just check if they have the same
  ;; base revision, or the target (right) base revision is a
  ;; descendant of the source.
  (message "checking if propagate needed")

  (let ((left-base (xmtn--get-base-revision-hash-id-or-null left-work))
        (right-base (xmtn--get-base-revision-hash-id-or-null right-work)))

    (if (string= left-base right-base)
        (error "don't need to propagate")
      ;; check for right descendant of left
      (let ((descendents (xmtn-automate-simple-command-output-lines left-work (list "descendents" left-base))))
        (while descendents
          (if (string= right-base (car descendents))
              (error "don't need to propagate"))
          (setq descendents (cdr descendents)))))
  ))

;;;###autoload
(defun xmtn-conflicts-propagate (left-work right-work)
  "List conflicts for a propagate from LEFT-WORK to RIGHT-WORK workspace branch head revisions.
Allow specifying resolutions.  LEFT-WORK and RIGHT-WORK are strings giving
workspace directories; prompted if nil. Review is done in RIGHT-WORK
workspace."
  (interactive "i\ni")
  (setq left-work (dvc-read-project-tree-maybe "Propagate from (workspace directory): " left-work))
  (setq right-work (dvc-read-project-tree-maybe "to (workspace directory): " right-work))

  (let ((left-branch (xmtn--tree-default-branch left-work))
        (right-branch (xmtn--tree-default-branch right-work)))

    (xmtn-check-workspace-for-propagate left-work left-branch)
    (xmtn-check-workspace-for-propagate right-work right-branch)

    (xmtn-check-propagate-needed left-work right-work)

    (message "computing conflicts")

    (xmtn-conflicts-1 left-work
                    (car (xmtn--heads left-work left-branch))
                    right-work
                    (car (xmtn--heads right-work right-branch)))))

;;;###autoload
(defun xmtn-conflicts-merge ()
  "List conflicts between current head revisions."
  (interactive)
  (let ((default-directory
          (dvc-read-project-tree-maybe "Review conflicts in (workspace directory): ")))
    (xmtn-conflicts-1 default-directory nil default-directory nil)))

;;;###autoload
(defun xmtn-conflicts-review (&optional workspace)
  "Review conflicts for WORKSPACE (a directory; default prompt)."
  (interactive)
  (let ((default-directory
          (dvc-read-project-tree-maybe "Review conflicts for (workspace directory): "
                                       (when workspace (expand-file-name workspace))))
        (file-name "_MTN/conflicts"))
    (if (not (file-exists-p file-name))
        (error "conflicts file not found"))

    (let ((conflicts-buffer (dvc-get-buffer-create 'xmtn 'conflicts default-directory)))
      (dvc-switch-to-buffer-maybe conflicts-buffer)
      (setq buffer-read-only nil)
      (xmtn-conflicts-load-opts)
      (set (make-local-variable 'after-insert-file-functions) '(xmtn-conflicts-after-insert-file))
      (insert-file-contents "_MTN/conflicts" t nil nil t))))

;;;###autoload
(defun xmtn-conflicts-clean (&optional workspace)
  "Remove conflicts resolution files from WORKSPACE (a directory; default prompt)."
  (interactive)
  (let ((default-directory
          (dvc-read-project-tree-maybe "Remove conflicts resolutions for (workspace directory): "
                                       (when workspace (expand-file-name workspace)))))

    (if (file-exists-p "_MTN/conflicts")
        (delete-file "_MTN/conflicts"))

    (if (file-exists-p xmtn-conflicts-opts-file)
        (delete-file xmtn-conflicts-opts-file))

    (if (file-exists-p "_MTN/resolutions")
        (dired-delete-file "_MTN/resolutions" 'always))

    (message "conflicts cleaned")
    ))

(provide 'xmtn-conflicts)

;; end of file
