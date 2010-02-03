;;; xmtn-automate.el --- Interface to monotone's "automate" functionality

;; Copyright (C) 2008, 2009 Stephen Leake
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

;; This library provides access to monotone's "automate" interface
;; from Emacs Lisp.
;;
;; see http://www.monotone.ca/docs/Automation.html#Automation for
;; details of the monotone automate command.
;;
;; mtn automate allows sending several commands to a single mtn
;; process, and provides the results in a form that is easy to
;; parse. It does some caching between command, and will do more in
;; the future, so this is a significant speed-up over spawning a new
;; subprocess for each command.
;;
;; To allow xmtn-automate to track how long an automate stdio process
;; needs to be kept around, and to store meta data, we introduce the
;; concept of a session.  To the programmer using this library, a
;; session is an opaque object that is needed to run automate
;; commands.  Each session is associated with a monotone workspace
;; ("root") that the commands will operate on.  A session can be
;; obtained using `xmtn-automate-cache-session'.  Note that
;; `xmtn-automate-cache-session' doesn't necessarily start a fresh
;; monotone process, if a session with that root already exists.  The
;; process must be killed with `xmtn-automate-kill-session'.
;;
;; Once you have a session object, you can use
;; `xmtn-automate-new-command' to send commands to monotone.
;;
;; A COMMAND is a list of strings (the command and its arguments), or
;; a cons of lists of strings. If car COMMAND is a list, car COMMAND is
;; options (without leading "--"), cdr is the command and arguments.
;;
;; `xmtn-automate-new-command' returns a command handle.  You use this
;; handle to check the error code of the command and obtain its
;; output.  Your Emacs Lisp code can also do other computation while
;; the monotone command runs.  Allowing this kind of parallelism and
;; incremental processing of command output is the main reason for
;; introducing command handles.
;;
;; The intention behind this protocol is to allow Emacs Lisp code to
;; process command output incrementally as it arrives instead of
;; waiting until it is complete.  However, for xmtn-basic-io, the
;; bookkeeping overhead for this kind of pipelining was excessive --
;; byte-compiled Emacs Lisp is rather slow.  But I didn't try very
;; hard to tune it, either.  So I'm not sure whether incremental
;; processing is useful.
;;
;; In the output buffer, the mtn stdio output header (<command
;; number>:<err code>:<last?>:<size>:<data>) has been processed;
;; only the data is present.

;; There are some notes on the design of xmtn in
;; docs/xmtn-readme.txt.

;;; Code:

(eval-and-compile
  (require 'cl)
  (require 'parse-time)                 ;for parse-integer
  (require 'xmtn-base)
  (require 'xmtn-run)
  (require 'xmtn-compat))

(defun xmtn-automate-command-error-code (command)
  (let ((process (xmtn-automate--session-process
                  (xmtn-automate--command-handle-session command))))
    (while (null (xmtn-automate--command-handle-error-code command))
      (xmtn--assert-for-effect
       (accept-process-output process))))
  (xmtn-automate--command-handle-error-code command))

(defun xmtn-automate-command-buffer (command)
  (xmtn-automate--command-handle-buffer command))

(defun xmtn-automate-command-write-marker-position (command)
  (marker-position (xmtn-automate--command-handle-write-marker command)))

(defun xmtn-automate-command-accept-output (command)
  (let ((previous-write-marker-position
         (marker-position (xmtn-automate--command-handle-write-marker
                           command))))
    (while (and (= (marker-position (xmtn-automate--command-handle-write-marker
                                     command))
                   previous-write-marker-position)
                (not (xmtn-automate--command-handle-finished-p command)))
      (xmtn--assert-for-effect
       (accept-process-output
        (xmtn-automate--session-process
         (xmtn-automate--command-handle-session command)))))
    (> (marker-position (xmtn-automate--command-handle-write-marker
                         command))
       previous-write-marker-position)))

(defun xmtn-automate-command-finished-p (command)
  (xmtn-automate--command-handle-finished-p command))

(defun xmtn-automate-command-wait-until-finished (handle)
  (while (not (xmtn-automate-command-finished-p handle))
    (xmtn--assert-for-effect (or (xmtn-automate-command-accept-output handle)
                                 (xmtn-automate-command-finished-p handle))))
  nil)

(defvar xmtn-automate--*sessions* '()
  "Assoc list of sessions, indexed by uniquified root directory.")

(defun xmtn-automate-cache-session (root)
  "If necessary, create a mtn automate session for workspace
ROOT, store it in session cache. Return session."
  ;; we require an explicit root argument here, rather than relying on
  ;; default-directory, because one application is to create several
  ;; sessions for several workspaces, and operate on them as a group
  ;; (see xmtn-multi-status.el, for example).
  (let* ((default-directory (dvc-uniquify-file-name root))
         (session (xmtn-automate-get-cached-session default-directory)))
    (or session
        (progn
          (setq session (xmtn-automate--make-session default-directory default-directory))
          (setq xmtn-automate--*sessions*
                (acons default-directory session xmtn-automate--*sessions*))
          session))))

(defun xmtn-automate-get-cached-session (key)
  "Return a session from the cache, or nil. KEY is uniquified
workspace root."
  (cdr (assoc key xmtn-automate--*sessions*)))

(defun xmtn-automate--command-output-as-string-ignoring-exit-code (handle)
  (xmtn-automate-command-wait-until-finished handle)
  (with-current-buffer (xmtn-automate-command-buffer handle)
    (prog1
        (buffer-substring-no-properties (point-min) (point-max))
      (xmtn-automate--cleanup-command handle))))

(defun xmtn-automate-command-check-for-and-report-error (handle)
  (unless (eql (xmtn-automate-command-error-code handle) 0)
    (error "mtn automate command (arguments %S) reported an error (code %s):\n%s"
           (xmtn-automate--command-handle-arguments handle)
           (xmtn-automate-command-error-code handle)
           (xmtn-automate--command-output-as-string-ignoring-exit-code handle)))
  nil)

(defun xmtn-automate-simple-command-output-string (root command)
  "Send COMMAND to session for ROOT. Return result as a string."
  (let* ((session (xmtn-automate-cache-session root))
         (command-handle (xmtn-automate--new-command session command nil)))
    (xmtn-automate-command-check-for-and-report-error command-handle)
    (xmtn-automate--command-output-as-string-ignoring-exit-code command-handle)))

(defun xmtn-automate-simple-command-output-insert-into-buffer
  (root buffer command)
  "Send COMMAND to session for ROOT, insert result into BUFFER."
  (let* ((session (xmtn-automate-cache-session root))
         (command-handle (xmtn-automate--new-command session command nil)))
    (xmtn-automate-command-check-for-and-report-error command-handle)
    (xmtn-automate-command-wait-until-finished command-handle)
    (with-current-buffer buffer
      (insert-buffer-substring-no-properties
       (xmtn-automate-command-buffer command-handle)))
    (xmtn-automate--cleanup-command command-handle)))

(defun xmtn-automate-command-output-lines (handle)
  "Return list of lines of output in HANDLE; first line output is
first in list."
  (xmtn-automate-command-check-for-and-report-error handle)
  (xmtn-automate-command-wait-until-finished handle)
  (save-excursion
    (set-buffer (xmtn-automate-command-buffer handle))
    (goto-char (point-min))
    (let (result)
      (while (< (point) (point-max))
        (setq result (cons (buffer-substring-no-properties
                            (point)
                            (progn (end-of-line) (point)))
                           result))
        (forward-line 1))
      (xmtn-automate--cleanup-command handle)
      (nreverse result))))

(defun xmtn-automate-simple-command-output-lines (root command)
  "Return list of strings containing output of COMMAND, one line per
string."
  (let* ((session (xmtn-automate-cache-session root))
         (command-handle (xmtn-automate--new-command session command nil)))
    (xmtn-automate-command-output-lines command-handle)))

(defun xmtn-automate-simple-command-output-line (root command)
  "Return the one line output from mtn automate as a string.

Signals an error if output contains zero lines or more than one line."
  (let ((lines (xmtn-automate-simple-command-output-lines root command)))
    (unless (eql (length lines) 1)
      (error "Expected precisely one line of output from mtn automate, got %s: %s %S"
             (length lines)
             xmtn-executable
             command))
    (first lines)))

(defun xmtn-automate--set-process-session (process session)
  (process-put process 'xmtn-automate--session session))

(defun xmtn-automate--process-session (process)
  (process-get process 'xmtn-automate--session))

(defstruct (xmtn-automate--decoder-state
            (:constructor xmtn-automate--%make-raw-decoder-state))
  (read-marker)
  (remaining-chars 0)
  (last-p nil))

(defstruct (xmtn-automate--session
            (:constructor xmtn-automate--%make-raw-session)
            (:copier xmtn-automate--copy-session))
  (root)
  (name)
  (buffer nil)
  (process nil)
  (decoder-state)
  (next-command-number 0)
  (must-not-kill-counter)
  (remaining-command-handles)
  (sent-kill-p)
  (closed-p nil))

(defstruct (xmtn-automate--command-handle
            (:constructor xmtn-automate--%make-raw-command-handle))
  (arguments)
  (mtn-command-number)
  (session-command-number)
  (session)
  (buffer)
  (write-marker)
  (may-kill-p)
  (finished-p nil)
  (error-code nil))

(defun* xmtn-automate--initialize-session (session &key root name)
  (xmtn--assert-optional (equal root (file-name-as-directory root)) t)
  (setf (xmtn-automate--session-root session) root
        (xmtn-automate--session-name session) name
        (xmtn-automate--session-process session) nil
        (xmtn-automate--session-closed-p session) nil)
  nil)

(defun xmtn-automate--make-session (root key)
  (dvc-trace "new session %s" key)
  (let* ((name (format "xmtn automate session for %s" key)))
    (let ((session (xmtn-automate--%make-raw-session)))
      (xmtn-automate--initialize-session session :root root :name name)
      session)))

(defun xmtn-automate--session-send-process-kill (session)
  (let ((process (xmtn-automate--session-process session)))
    ;; Stop parser.
    (setf (xmtn-automate--session-sent-kill-p session) t)
    (with-current-buffer (xmtn-automate--session-buffer session)
      (let ((inhibit-read-only t)
            deactivate-mark)
        (save-excursion
          (goto-char (process-mark process))
          (insert "\n(killing process)\n")
          (set-marker (process-mark process) (point)))))
    ;; Maybe this should really be a sigpipe.  But let's not get too
    ;; fancy (ha!) and non-portable.
    ;;(signal-process (xmtn-automate--session-process session) 'PIPE)
    ;; This call to `sit-for' is apparently needed in some situations to
    ;; make sure the process really gets killed.
    (sit-for 0)
    (interrupt-process process))
  nil)

(defun xmtn-automate--close-session (session)
  "Kill session process, buffer."
  (setf (xmtn-automate--session-closed-p session) t)
  (let ((process (xmtn-automate--session-process session)))
    (cond
     ((null process)
      ;; Process died for some reason - most likely 'mtn not found in
      ;; path'. Don't warn if buffer hasn't been deleted; that
      ;; obscures the real error message
      nil)
     ((ecase (process-status process)
        (run nil)
        (exit t)
        (signal t))
      (unless xmtn-automate--*preserve-buffers-for-debugging*
        (kill-buffer (xmtn-automate--session-buffer session))))
     (t
      (process-send-eof process)
      (if (zerop (xmtn-automate--session-must-not-kill-counter session))
          (xmtn-automate--session-send-process-kill session)
        ;; We can't kill the buffer yet.  We need to dump mtn's output
        ;; in there so we can parse it and determine when the critical
        ;; commands are finished so we can then kill mtn.
        (dvc-trace
         "Not killing process %s yet: %s out of %s remaining commands are critical"
         (process-name process)
         (xmtn-automate--session-must-not-kill-counter session)
         (length (xmtn-automate--session-remaining-command-handles session))))
      (with-current-buffer (xmtn-automate--session-buffer session)
        ;; This isn't essential but helps debugging.
        (rename-buffer (format "*%s: killed session*"
                               (xmtn-automate--session-name session))
                       t))
      (let ((fake-session (xmtn-automate--copy-session session)))
        (xmtn-automate--set-process-session process fake-session)))))
  nil)

(defun xmtn-automate--start-process (session)
  (xmtn--check-cached-command-version)
  (xmtn--assert-optional (not (xmtn-automate--session-closed-p session)))
  (xmtn--assert-optional (typep session 'xmtn-automate--session))
  (let ((name (xmtn-automate--session-name session))
        (buffer (xmtn-automate--new-buffer session))
        (root (xmtn-automate--session-root session)))
    (let ((process-connection-type nil)
          (default-directory root))
      (let ((process
             (apply 'start-process name buffer xmtn-executable
                    "automate" "stdio" xmtn-additional-arguments)))
        (xmtn-automate--set-process-session process session)
        (set-process-filter process 'xmtn-automate--process-filter)
        (set-process-sentinel process 'xmtn-automate--process-sentinel)
        (xmtn--set-process-query-on-exit-flag process nil)
        ;; Need binary (or no-conversion or maybe raw-text-unix?)
        ;; since this is the format in which mtn automate stdio
        ;; computes the size of the output.
        (set-process-coding-system process 'binary 'binary)
        (setf (xmtn-automate--session-process session) process)
        (setf (xmtn-automate--session-decoder-state session)
              (xmtn-automate--%make-raw-decoder-state
               :read-marker (with-current-buffer buffer
                              (xmtn--assert-optional (eql (point-min) (point)) t)
                              (set-marker (make-marker)
                                          (point-min)))))
        (setf (xmtn-automate--session-must-not-kill-counter session) 0)
        (setf (xmtn-automate--session-remaining-command-handles session) (list))
        (setf (xmtn-automate--session-sent-kill-p session) nil)
        process))))

(defun xmtn-automate--ensure-process (session)
  "Ensure SESSION has an active process; restart it if it died."
  (let ((process (xmtn-automate--session-process session)))
    (when (or (null process)
              (ecase (process-status process)
                (run nil)
                (exit t)
                (signal t)))
      (setq process (xmtn-automate--start-process session))
      (setf (xmtn-automate--session-process session) process))
    (xmtn--assert-optional (buffer-live-p (xmtn-automate--session-buffer
                                           session)))
    process))

(defun xmtn-automate--new-buffer (session)
  (let* ((buffer-base-name (format " *%s: session*"
                                   (xmtn-automate--session-name session)))
         (buffer (generate-new-buffer buffer-base-name)))
    (with-current-buffer buffer
      (buffer-disable-undo)
      (set-buffer-multibyte nil)
      (setq buffer-read-only t))
    (setf (xmtn-automate--session-buffer session) buffer)
    buffer))

(defun xmtn-automate--append-encoded-strings (strings)
  "Encode STRINGS (a list of strings or nil) in automate stdio format,
insert into current buffer.  Assumes that point is at the end of
the buffer."
  (xmtn--assert-optional (eql (point) (point-max)))
  (dolist (string strings)
    (if string
        (progn
          (save-excursion (insert string))
          (encode-coding-region (point) (point-max) 'xmtn--monotone-normal-form)
          (insert (number-to-string (- (point-max) (point))) ":")
          (goto-char (point-max)))))
  nil)

(defun xmtn-automate--send-command-string (session command option-plist session-number)
  "Send COMMAND and OPTION-PLIST to SESSION."
  (let* ((buffer-name (format "*%s: input for command %s*"
                              (xmtn-automate--session-name session)
                              session-number))
         (buffer nil))
    (unwind-protect
        (progn
          (when (get-buffer buffer-name)
            ;; Make sure the buffer is in a clean state.
            (with-current-buffer buffer-name
              (let ((inhibit-read-only t))
                (erase-buffer))
              (fundamental-mode)))
          (setq buffer (get-buffer-create buffer-name))
          (with-current-buffer buffer
            (buffer-disable-undo)
            (set-buffer-multibyte t)
            (setq buffer-read-only t)
            (let ((inhibit-read-only t))
              (when option-plist
                (insert "o")
                (xmtn-automate--append-encoded-strings option-plist)
                (insert "e"))
              (insert "l")
              (xmtn-automate--append-encoded-strings command)
              (insert "e\n"))

            (dvc-trace "mtn automate: '%s'" (buffer-substring (point-min) (point-max)))

            (process-send-region (xmtn-automate--session-process session)
                                 (point-min) (point-max))))
      (when buffer
        (unless xmtn-automate--*preserve-buffers-for-debugging*
          (kill-buffer buffer))))))

(defun xmtn-automate--new-command (session command may-kill-p)
  "Send COMMAND to SESSION."
  (xmtn-automate--ensure-process session)
  (let* ((command-number
          (1- (incf (xmtn-automate--session-next-command-number
                     session))))
         (buffer-name (format " *%s: output for command %s*"
                              (xmtn-automate--session-name session)
                              command-number))
         (buffer
          (progn (when (get-buffer buffer-name)
                   ;; Make sure no local variables or mode changes
                   ;; remain from the previous command parser.
                   (with-current-buffer buffer-name
                     (let ((inhibit-read-only t))
                       (erase-buffer))
                     (fundamental-mode)))
                 (get-buffer-create buffer-name))))
    (if (not (listp (car command)))
        (xmtn-automate--send-command-string session command '() command-number)
      (xmtn-automate--send-command-string session (cdr command) (car command) command-number))
    (with-current-buffer buffer
      (buffer-disable-undo)
      (set-buffer-multibyte nil)
      (setq buffer-read-only t)
      (xmtn--assert-optional (and (eql (point) (point-min))
                                  (eql (point) (point-max))))
      (let ((handle (xmtn-automate--%make-raw-command-handle
                     :session session
                     :arguments command
                     :session-command-number command-number
                     :may-kill-p may-kill-p
                     :buffer buffer
                     :write-marker (set-marker (make-marker) (point)))))
        (setf
         (xmtn-automate--session-remaining-command-handles session)
         (nconc (xmtn-automate--session-remaining-command-handles session)
                (list handle)))
        (when (not may-kill-p)
          (incf (xmtn-automate--session-must-not-kill-counter session))
          (xmtn--set-process-query-on-exit-flag
           (xmtn-automate--session-process session)
           t))
        handle))))

(defun xmtn-automate--cleanup-command (handle)
  (unless xmtn-automate--*preserve-buffers-for-debugging*
    (kill-buffer (xmtn-automate--command-handle-buffer handle))))

(defsubst xmtn-automate--process-new-output--copy (session)
  (let* ((session-buffer (xmtn-automate--session-buffer session))
         (state (xmtn-automate--session-decoder-state session))
         (read-marker (xmtn-automate--decoder-state-read-marker state))
         (command (first (xmtn-automate--session-remaining-command-handles
                          session)))
         (command-output-buffer
          (xmtn-automate--command-handle-buffer command))
         (write-marker
          (xmtn-automate--command-handle-write-marker command)))
    (xmtn--assert-optional (not (xmtn-automate--session-sent-kill-p session)))
    (with-current-buffer session-buffer
      (let* ((end (min (+ read-marker
                          (xmtn-automate--decoder-state-remaining-chars state))
                       (point-max)))
             (chars-to-read (- end read-marker)))
        (cond
         ((= chars-to-read 0)
          nil)
         ((> chars-to-read 0)
          (if (not (buffer-live-p command-output-buffer))
              ;; Buffer has already been killed, just discard input.
              (progn)
            (with-current-buffer command-output-buffer
              (save-excursion
                (goto-char write-marker)
                (let ((inhibit-read-only t)
                      deactivate-mark)
                  (insert-buffer-substring-no-properties session-buffer
                                                         read-marker
                                                         end))
                (set-marker write-marker (point))))
            ;;(xmtn--debug-mark-text-processed session-buffer read-marker end nil)
            )
          (set-marker read-marker end)
          (decf (xmtn-automate--decoder-state-remaining-chars state)
                chars-to-read)
          t)
         (t (xmtn--assert-nil))))))
  ;; Return value matters!
  )

(defun xmtn--debug-mark-text-processed (buffer start end bold-p)
  (xmtn--assert-optional (< start end) t)
  (with-current-buffer buffer
    (let ((inhibit-read-only t))
      (if bold-p
          (xmtn--assert-for-effect
           (add-text-properties start end
                                '(face
                                  (:strike-through
                                   t
                                   :weight semi-bold))))
        (xmtn--assert-for-effect
         (add-text-properties start end '(face (:strike-through
                                                t))))))))

(defsubst xmtn-automate--process-new-output (session new-string)
  (let* ((session-buffer (xmtn-automate--session-buffer session))
         (state (xmtn-automate--session-decoder-state session))
         (read-marker (xmtn-automate--decoder-state-read-marker state))
         (write-marker (process-mark (xmtn-automate--session-process session)))
         (tag 'check-for-more))
    (with-current-buffer session-buffer
      ;; Why oh why doesn't (require 'cl) provide tagbody...
      (loop
       for command = (first (xmtn-automate--session-remaining-command-handles
                             session))
       do
       (xmtn--assert-optional (or (eql tag 'exit-loop)
                                  (not (xmtn-automate--session-sent-kill-p
                                        session))))
       (ecase tag
         (check-for-more
          (xmtn--assert-optional (<= read-marker write-marker) t)
          (if (= read-marker write-marker)
              (setq tag 'exit-loop)
            (setq tag 'again)))
         (again
          (cond
           ((> (xmtn-automate--decoder-state-remaining-chars state) 0)
            (if (xmtn-automate--process-new-output--copy session)
                (setq tag 'again)
              (setq tag 'check-for-more)))
           ((and (= (xmtn-automate--decoder-state-remaining-chars state) 0)
                 (xmtn-automate--decoder-state-last-p state))
            (xmtn--assert-optional command)
            (setf (xmtn-automate--command-handle-finished-p command) t)
            (with-no-warnings
              ;; discard result
              (pop (xmtn-automate--session-remaining-command-handles session)))
            (setq tag 'check-for-more)
            (when (not (xmtn-automate--command-handle-may-kill-p command))
              (when (zerop (decf (xmtn-automate--session-must-not-kill-counter
                                  session)))
                (xmtn--set-process-query-on-exit-flag
                 (xmtn-automate--session-process session)
                 nil)
                (when (xmtn-automate--session-closed-p session)
                  (xmtn-automate--session-send-process-kill session)
                  (setq tag 'exit-loop))))
            (setf (xmtn-automate--decoder-state-last-p state) nil))
           ((and (= (xmtn-automate--decoder-state-remaining-chars state) 0)
                 (not (xmtn-automate--decoder-state-last-p state)))
            (unless command
              (error "Unexpected output from mtn: %s" new-string))
            (save-excursion
              (goto-char read-marker)
              (cond ((looking-at
                      "\\([0-9]+\\):\\([012]\\):\\([lm]\\):\\([0-9]+\\):")
                     (let ((command-number (parse-integer (match-string 1)))
                           (error-code (parse-integer (match-string 2)))
                           (last-p (cond
                                    ((string= (match-string 3) "l") t)
                                    ((string= (match-string 3) "m") nil)
                                    (t (xmtn--assert-nil))))
                           (size (parse-integer (match-string 4))))
                       (xmtn--assert-optional (typep command-number
                                                     '(integer 0 *))
                                              t)
                       (xmtn--assert-optional (typep error-code '(member 0 1 2))
                                              t)
                       (xmtn--assert-optional (typep size '(integer 0 *)) t)
                       (xmtn--assert-optional
                        (eql
                         command-number
                         (xmtn-automate--command-handle-mtn-command-number
                          command)))
                       (setf (xmtn-automate--command-handle-error-code command)
                             error-code)
                       (setf (xmtn-automate--decoder-state-remaining-chars
                              state)
                             size)
                       (setf (xmtn-automate--decoder-state-last-p state)
                             last-p)
                       ;;(xmtn--debug-mark-text-processed session-buffer
                       ;;                                 read-marker
                       ;;                                 (match-end 0)
                       ;;                                 t)
                       (set-marker read-marker (match-end 0)))
                     (setq tag 'again))
                    ;; This is just a simple heuristic, there are many
                    ;; kinds of invalid input that it doesn't detect.
                    ;; FIXME: This can errorneously be triggered by
                    ;; warnings that mtn prints on stderr; but Emacs
                    ;; interleaves stdout and stderr (see (elisp)
                    ;; Output from Processes) with no way to
                    ;; distinguish between them.  We'll probably have
                    ;; to spawn mtn inside a shell that redirects
                    ;; stderr to a file.  But I don't think that's
                    ;; possible in a portable way...
                    ((looking-at "[^0-9]")
                     (error "Invalid output from mtn: %s"
                            (buffer-substring-no-properties (point)
                                                            (point-max))))
                    (t
                     (xmtn--assert-optional command)
                     (setq tag 'exit-loop)))))
           (t (xmtn--assert-nil))))
         (exit-loop (return))))))
  nil)


(defvar xmtn-automate--*preserve-buffers-for-debugging* nil)

(defun xmtn-automate--process-sentinel (process event-string)
  (let ((status (process-status process))
        (session (xmtn-automate--process-session process)))
    (let ((buffer (xmtn-automate--session-buffer session)))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (let ((inhibit-read-only t)
                deactivate-mark)
            (save-excursion
              ;; This seems to fail in XEmacs when running the test
              ;; `file-diff'.  I don't know why.
              (xmtn--assert-optional (marker-position (process-mark process))
                                     t)
              (goto-char (process-mark process))
              (insert (format "\n(process exited: %S)\n"
                              (if (eql (aref event-string
                                             (1- (length event-string)))
                                       ?\n)
                                  (subseq event-string 0
                                          (1- (length event-string)))
                                event-string)))
              (set-marker (process-mark process) (point))))))
      (flet ((reclaim-buffer ()
               (unless xmtn-automate--*preserve-buffers-for-debugging*
                 ;; Maybe it's not such a good idea to kill the buffer
                 ;; from here since that will run `kill-buffer-hook',
                 ;; and the functions in there might not be prepared to
                 ;; run inside a sentinel.  But let's wait until someone
                 ;; actually encounters this problem.
                 (kill-buffer buffer)
                 )))
        (ecase status
          (exit
           (xmtn--assert-optional (eql (process-exit-status process) 0) t)
           (reclaim-buffer))
          (signal
           (if (xmtn-automate--session-sent-kill-p session)
               (reclaim-buffer)
             (message "Process %s died due to signal" (process-name process))
             (when (not (zerop (xmtn-automate--session-must-not-kill-counter
                                session)))
               (lwarn
                'xmtn ':error
                "Process %s died due to signal during a critical operation"
                (process-name process))))))))))

(defun xmtn-automate--process-filter (process input-string)
  (let ((session (xmtn-automate--process-session process)))
    (let ((buffer (xmtn-automate--session-buffer session)))
      (xmtn--assert-optional (eql (process-buffer process) buffer))
      (xmtn--assert-optional (buffer-live-p buffer))
      (with-current-buffer buffer
        (let* ((mark (process-mark process))
               (move-point-p (= (point) mark)))
          (save-excursion
            (goto-char mark)
            (let ((inhibit-read-only t)
                  deactivate-mark)
              (insert input-string))
            (set-marker mark (point)))
          (when move-point-p (goto-char mark))))
      ;;(with-local-quit                    ; For debugging.
      ;; Emacs receives a message "mtn: operation canceled: Interrupt"
      ;; from mtn after we kill it.  Ignore such "input".
      (unless (xmtn-automate--session-sent-kill-p session)
        (xmtn-automate--process-new-output session input-string))
      ;;)
      )))

(defun xmtn--map-parsed-certs (xmtn--root xmtn--revision-hash-id xmtn--thunk)
  (lexical-let ((root xmtn--root)
                (revision-hash-id xmtn--revision-hash-id)
                (thunk xmtn--thunk))
    (xmtn--with-automate-command-output-basic-io-parser
        (xmtn--next-stanza root `("certs" ,revision-hash-id))
      (loop
       for xmtn--stanza = (funcall xmtn--next-stanza)
       while xmtn--stanza
       do (xmtn-match xmtn--stanza
            ((("key" (string $xmtn--key))
              ("signature" (string $xmtn--signature))
              ("name" (string $xmtn--name))
              ("value" (string $xmtn--value))
              ("trust" (string $xmtn--trust)))
             (setq xmtn--signature (xmtn-match xmtn--signature
                                     ("ok" 'ok)
                                     ("bad" 'bad)
                                     ("unknown" 'unknown)))
             (let ((xmtn--trusted (xmtn-match xmtn--trust
                                    ("trusted" t)
                                    ("untrusted" nil))))
               (macrolet ((decodef (var)
                            `(setq ,var (decode-coding-string
                                         ,var 'xmtn--monotone-normal-form))))
                 (decodef xmtn--key)
                 (decodef xmtn--name)
                 ;; I'm not sure this is correct.  The documentation
                 ;; mentions a cert_is_binary hook, but it doesn't
                 ;; exist; and even if it did, we would have no way of
                 ;; calling it from here.  But, since cert values are
                 ;; always passed on the command line, and command
                 ;; line arguments are converted to utf-8, I suspect
                 ;; certs will also always be in utf-8.
                 (decodef xmtn--value))
               (funcall thunk
                        xmtn--key xmtn--signature xmtn--name xmtn--value
                        xmtn--trusted))))))))

(defun xmtn--list-parsed-certs (root revision-hash-id)
  "Return a list of the contents of each cert attached to REVISION-HASH-ID.
Each element of the list is a list; key, signature, name, value, trust."
  (lexical-let ((accu '()))
    (xmtn--map-parsed-certs root revision-hash-id
                            (lambda (key signature name value trusted)
                              (push (list key signature name value trusted)
                                    accu)))
    (setq accu (nreverse accu))
    accu))

(defun xmtn--heads (root branch)
  ;; apparently stdio automate doesn't default arguments properly;
  ;; this fails if branch is not passed to mtn.
  (xmtn-automate-simple-command-output-lines root (list "heads"
                                                        (or branch
                                                            (xmtn--tree-default-branch root)))))

(defun xmtn--tree-default-branch (root)
  (xmtn-automate-simple-command-output-line root `("get_option" "branch")))

(defun xmtn-automate-local-changes (work)
  "Summary of status  for WORK; 'ok if no changes, 'need-commit if changes."
  (message "checking %s for local changes" work)
  (let ((default-directory work))

    (let ((result (xmtn-automate-simple-command-output-string
                   default-directory
                   (list (list "no-unchanged" "no-ignored")
                         "inventory"))))
     (if (> (length result) 0)
         'need-commit
       'ok))))

(provide 'xmtn-automate)

;;; xmtn-automate.el ends here
