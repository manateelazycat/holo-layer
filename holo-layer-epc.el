;;; epcs.el --- EPC Server              -*- lexical-binding: t -*-

;; Copyright (C) 2011,2012,2013  Masashi Sakurai

;; Author: Masashi Sakurai <m.sakurai at kiwanami.net>
;; Keywords: lisp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'cl-lib)
(require 'subr-x)

;; deferred
(cl-defmacro holo-layer-deferred-chain (&rest elements)
  "Anaphoric function chain macro for deferred chains."
  (declare (debug (&rest form))
           (indent 0))
  `(let (it)
     ,@(cl-loop for i in elements
                collect
                `(setq it ,i))
     it))

;; Debug
(defvar holo-layer-deferred-debug nil
  "Debug output switch.")

(defvar holo-layer-deferred-debug-count 0
  "[internal] Debug output counter.")

(defun holo-layer-deferred-log (&rest args)
  "[internal] Debug log function."
  (when holo-layer-deferred-debug
    (with-current-buffer (get-buffer-create "*holo-layer-deferred-log*")
      (save-excursion
        (goto-char (point-max))
        (insert (format "%5i %s\n\n\n" holo-layer-deferred-debug-count (apply #'format args)))))
    (cl-incf holo-layer-deferred-debug-count)))

(defvar holo-layer-deferred-debug-on-signal nil
  "If non nil, the value `debug-on-signal' is substituted this
value in the `condition-case' form in deferred
implementations. Then, Emacs debugger can catch an error occurred
in the asynchronous tasks.")

(cl-defmacro holo-layer-deferred-condition-case (var protected-form &rest handlers)
  "[internal] Custom condition-case. See the comment for
`holo-layer-deferred-debug-on-signal'."
  (declare (debug condition-case)
           (indent 1))
  `(let ((debug-on-signal
          (or debug-on-signal holo-layer-deferred-debug-on-signal)))
     (condition-case ,var
         ,protected-form
       ,@handlers)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Back end functions of deferred tasks

(defvar holo-layer-deferred-tick-time 0.001
  "Waiting time between asynchronous tasks (second).
The shorter waiting time increases the load of Emacs. The end
user can tune this parameter. However, applications should not
modify it because the applications run on various environments.")

(defvar holo-layer-deferred-queue nil
  "[internal] The execution queue of deferred objects.
See the functions `holo-layer-deferred-post-task' and `holo-layer-deferred-worker'.")

(defun holo-layer-deferred-post-task (d which &optional arg)
  "[internal] Add a deferred object to the execution queue
`holo-layer-deferred-queue' and schedule to execute.
D is a deferred object. WHICH is a symbol, `ok' or `ng'. ARG is
an argument value for execution of the deferred task."
  (let ((pack `(,d ,which . ,arg)))
    (push pack holo-layer-deferred-queue)
    (holo-layer-deferred-log "QUEUE-POST [%s]: %s" (length holo-layer-deferred-queue) pack)
    (run-at-time holo-layer-deferred-tick-time nil 'holo-layer-deferred-worker)
    d))

(defun holo-layer-deferred-worker ()
  "[internal] Consume a deferred task.
Mainly this function is called by timer asynchronously."
  (when holo-layer-deferred-queue
    (let* ((pack (car (last holo-layer-deferred-queue)))
           (d (car pack))
           (which (cadr pack))
           (arg (cddr pack)) value)
      (setq holo-layer-deferred-queue (nbutlast holo-layer-deferred-queue))
      (condition-case err
          (setq value (holo-layer-deferred-exec-task d which arg))
        (error
         (holo-layer-deferred-log "ERROR : %s" err)
         (message "deferred error : %s" err)))
      value)))

;; Struct: holo-layer-deferred-object
;;
;; callback    : a callback function (default `identity')
;; errorback   : an errorback function (default `holo-layer-deferred-resignal')
;; cancel      : a canceling function (default `holo-layer-deferred-default-cancel')
;; next        : a next chained deferred object (default nil)
;; status      : if 'ok or 'ng, this deferred has a result (error) value. (default nil)
;; value       : saved value (default nil)
;;
(cl-defstruct holo-layer-deferred-object
  (callback 'identity)
  (errorback 'holo-layer-deferred-resignal)
  (cancel 'holo-layer-deferred-default-cancel)
  next status value)

(defun holo-layer-deferred-resignal (err)
  "[internal] Safely resignal ERR as an Emacs condition.

If ERR is a cons (ERROR-SYMBOL . DATA) where ERROR-SYMBOL has an
`error-conditions' property, it is re-signaled unchanged. If ERR
is a string, it is signaled as a generic error using `error'.
Otherwise, ERR is formatted into a string as if by `print' before
raising with `error'."
  (cond ((and (listp err)
              (symbolp (car err))
              (get (car err) 'error-conditions))
         (signal (car err) (cdr err)))
        ((stringp err)
         (error "%s" err))
        (t
         (error "%S" err))))

(defun holo-layer-deferred-default-cancel (d)
  "[internal] Default canceling function."
  (holo-layer-deferred-log "CANCEL : %s" d)
  (setf (holo-layer-deferred-object-callback d) 'identity)
  (setf (holo-layer-deferred-object-errorback d) 'holo-layer-deferred-resignal)
  (setf (holo-layer-deferred-object-next d) nil)
  d)

(defun holo-layer-deferred-exec-task (d which &optional arg)
  "[internal] Executing deferred task. If the deferred object has
next deferred task or the return value is a deferred object, this
function adds the task to the execution queue.
D is a deferred object. WHICH is a symbol, `ok' or `ng'. ARG is
an argument value for execution of the deferred task."
  (holo-layer-deferred-log "EXEC : %s / %s / %s" d which arg)
  (when (null d) (error "holo-layer-deferred-exec-task was given a nil."))
  (let ((callback (if (eq which 'ok)
                      (holo-layer-deferred-object-callback d)
                    (holo-layer-deferred-object-errorback d)))
        (next-deferred (holo-layer-deferred-object-next d)))
    (cond
     (callback
      (holo-layer-deferred-condition-case err
                                         (let ((value (funcall callback arg)))
                                           (cond
                                            ((holo-layer-deferred-object-p value)
                                             (holo-layer-deferred-log "WAIT NEST : %s" value)
                                             (if next-deferred
                                                 (holo-layer-deferred-set-next value next-deferred)
                                               value))
                                            (t
                                             (if next-deferred
                                                 (holo-layer-deferred-post-task next-deferred 'ok value)
                                               (setf (holo-layer-deferred-object-status d) 'ok)
                                               (setf (holo-layer-deferred-object-value d) value)
                                               value))))
                                         (error
                                          (cond
                                           (next-deferred
                                            (holo-layer-deferred-post-task next-deferred 'ng err))
                                           (t
                                            (holo-layer-deferred-log "ERROR : %S" err)
                                            (message "deferred error : %S" err)
                                            (setf (holo-layer-deferred-object-status d) 'ng)
                                            (setf (holo-layer-deferred-object-value d) err)
                                            err)))))
     (t                                 ; <= (null callback)
      (cond
       (next-deferred
        (holo-layer-deferred-exec-task next-deferred which arg))
       ((eq which 'ok) arg)
       (t                               ; (eq which 'ng)
        (holo-layer-deferred-resignal arg)))))))

(defun holo-layer-deferred-set-next (prev next)
  "[internal] Connect deferred objects."
  (setf (holo-layer-deferred-object-next prev) next)
  (cond
   ((eq 'ok (holo-layer-deferred-object-status prev))
    (setf (holo-layer-deferred-object-status prev) nil)
    (let ((ret (holo-layer-deferred-exec-task
                next 'ok (holo-layer-deferred-object-value prev))))
      (if (holo-layer-deferred-object-p ret) ret
        next)))
   ((eq 'ng (holo-layer-deferred-object-status prev))
    (setf (holo-layer-deferred-object-status prev) nil)
    (let ((ret (holo-layer-deferred-exec-task next 'ng (holo-layer-deferred-object-value prev))))
      (if (holo-layer-deferred-object-p ret) ret
        next)))
   (t
    next)))

(defun holo-layer-deferred-new (&optional callback)
  "Create a deferred object."
  (if callback
      (make-holo-layer-deferred-object :callback callback)
    (make-holo-layer-deferred-object)))

(defun holo-layer-deferred-callback (d &optional arg)
  "Start deferred chain with a callback message."
  (holo-layer-deferred-exec-task d 'ok arg))

(defun holo-layer-deferred-errorback (d &optional arg)
  "Start deferred chain with an errorback message."
  (declare (indent 1))
  (holo-layer-deferred-exec-task d 'ng arg))

(defun holo-layer-deferred-callback-post (d &optional arg)
  "Add the deferred object to the execution queue."
  (declare (indent 1))
  (holo-layer-deferred-post-task d 'ok arg))

(defun holo-layer-deferred-next (&optional callback arg)
  "Create a deferred object and schedule executing. This function
is a short cut of following code:
 (holo-layer-deferred-callback-post (holo-layer-deferred-new callback))."
  (let ((d (if callback
               (make-holo-layer-deferred-object :callback callback)
             (make-holo-layer-deferred-object))))
    (holo-layer-deferred-callback-post d arg)
    d))

(defun holo-layer-deferred-nextc (d callback)
  "Create a deferred object with OK callback and connect it to the given deferred object."
  (declare (indent 1))
  (let ((nd (make-holo-layer-deferred-object :callback callback)))
    (holo-layer-deferred-set-next d nd)))

(defun holo-layer-deferred-error (d callback)
  "Create a deferred object with errorback and connect it to the given deferred object."
  (declare (indent 1))
  (let ((nd (make-holo-layer-deferred-object :errorback callback)))
    (holo-layer-deferred-set-next d nd)))

(defvar holo-layer-epc-debug nil)

(defun holo-layer-epc-log (&rest args)
  (when holo-layer-epc-debug
    (with-current-buffer (get-buffer-create "*holo-layer-epc-log*")
      (buffer-disable-undo)
      (goto-char (point-max))
      (insert (apply 'format args) "\n\n\n"))))

(defun holo-layer-epc-make-procbuf (name)
  "[internal] Make a process buffer."
  (let ((buf (get-buffer-create name)))
    (with-current-buffer buf
      (set (make-local-variable 'kill-buffer-query-functions) nil)
      (erase-buffer) (buffer-disable-undo))
    buf))

(defvar holo-layer-epc-uid 1)

(defun holo-layer-epc-uid ()
  (cl-incf holo-layer-epc-uid))

(defvar holo-layer-epc-accept-process-timeout 150
  "Asynchronous timeout time. (msec)")

(put 'epc-error 'error-conditions '(error epc-error))
(put 'epc-error 'error-message "EPC Error")

(cl-defstruct holo-layer-epc-connection
  "Set of information for network connection and event handling.

name    : Connection name. This name is used for process and buffer names.
process : Connection process object.
buffer  : Working buffer for the incoming data.
channel : Event channels for incoming messages."
  name process buffer channel)

(defun holo-layer-epc-connect (host port)
  "[internal] Connect the server, initialize the process and
return holo-layer-epc-connection object."
  (holo-layer-epc-log ">> Connection start: %s:%s" host port)
  (let* ((connection-id (holo-layer-epc-uid))
         (connection-name (format "holo-layer-epc con %s" connection-id))
         (connection-buf (holo-layer-epc-make-procbuf (format "*%s*" connection-name)))
         (connection-process
          (open-network-stream connection-name connection-buf host port))
         (channel (list connection-name nil))
         (connection (make-holo-layer-epc-connection
                      :name connection-name
                      :process connection-process
                      :buffer connection-buf
                      :channel channel)))
    (holo-layer-epc-log ">> Connection establish")
    (set-process-coding-system  connection-process 'binary 'binary)
    (set-process-filter connection-process
                        (lambda (p m)
                          (holo-layer-epc-process-filter connection p m)))
    (set-process-sentinel connection-process
                          (lambda (p e)
                            (holo-layer-epc-process-sentinel connection p e)))
    (set-process-query-on-exit-flag connection-process nil)
    connection))

(defun holo-layer-epc-process-sentinel (connection process msg)
  (holo-layer-epc-log "!! Process Sentinel [%s] : %S : %S"
                     (holo-layer-epc-connection-name connection) process msg)
  (holo-layer-epc-disconnect connection))

(defun holo-layer-epc-net-send (connection sexp)
  (let* ((msg (encode-coding-string
               (concat (holo-layer-epc-prin1-to-string sexp) "\n") 'utf-8-unix))
         (string (concat (format "%06x" (length msg)) msg))
         (proc (holo-layer-epc-connection-process connection)))
    (holo-layer-epc-log ">> SEND : [%S]" string)
    (process-send-string proc string)))

(defun holo-layer-epc-disconnect (connection)
  (let ((process (holo-layer-epc-connection-process connection))
        (buf (holo-layer-epc-connection-buffer connection))
        (name (holo-layer-epc-connection-name connection)))
    (holo-layer-epc-log "!! Disconnect [%s]" name)
    (when process
      (set-process-sentinel process nil)
      (delete-process process)
      (when (get-buffer buf) (kill-buffer buf)))
    (holo-layer-epc-log "!! Disconnected finished [%s]" name)))

(defun holo-layer-epc-process-filter (connection process message)
  (holo-layer-epc-log "INCOMING: [%s] [%S]" (holo-layer-epc-connection-name connection) message)
  (with-current-buffer (holo-layer-epc-connection-buffer connection)
    (goto-char (point-max))
    (insert message)
    (holo-layer-epc-process-available-input connection process)))

(defun holo-layer-epc-signal-connect (channel event-sym &optional callback)
  "Append an observer for EVENT-SYM of CHANNEL and return a deferred object.
If EVENT-SYM is `t', the observer receives all signals of the channel.
If CALLBACK function is given, the deferred object executes the
CALLBACK function asynchronously. One can connect subsequent
tasks to the returned deferred object."
  (let ((d (if callback
               (holo-layer-deferred-new callback)
             (holo-layer-deferred-new))))
    (push (cons event-sym d)
          (cddr channel))
    d))

(defun holo-layer-epc-signal-send (channel event-sym &rest args)
  "Send a signal to CHANNEL. If ARGS values are given,
observers can get the values by following code:

  (lambda (event)
    (destructuring-bind
     (event-sym (args))
     event ... ))
"
  (let ((observers (cddr channel))
        (event (list event-sym args)))
    (cl-loop for i in observers
             for name = (car i)
             for d = (cdr i)
             if (or (eq event-sym name) (eq t name))
             do (holo-layer-deferred-callback-post d event))))

(defun holo-layer-epc-process-available-input (connection process)
  "Process all complete messages that have arrived from Lisp."
  (with-current-buffer (process-buffer process)
    (while (holo-layer-epc-net-have-input-p)
      (let ((event (holo-layer-epc-net-read-or-lose process))
            (ok nil))
        (holo-layer-epc-log "<< RECV [%S]" event)
        (unwind-protect
            (condition-case err
                (progn
                  (apply 'holo-layer-epc-signal-send
                         (cons (holo-layer-epc-connection-channel connection) event))
                  (setq ok t))
              ('error (holo-layer-epc-log "MsgError: %S / <= %S" err event)))
          (unless ok
            (holo-layer-epc-process-available-input connection process)))))))

(defun holo-layer-epc-net-have-input-p ()
  "Return true if a complete message is available."
  (goto-char (point-min))
  (and (>= (buffer-size) 6)
       (>= (- (buffer-size) 6) (holo-layer-epc-net-decode-length))))

(defun holo-layer-epc-net-read-or-lose (_process)
  (condition-case error
      (holo-layer-epc-net-read)
    (error
     (debug 'error error)
     (error "net-read error: %S" error))))

(defun holo-layer-epc-net-read ()
  "Read a message from the network buffer."
  (goto-char (point-min))
  (let* ((length (holo-layer-epc-net-decode-length))
         (start (+ 6 (point)))
         (end (+ start length))
         _content)
    (cl-assert (cl-plusp length))
    (prog1 (save-restriction
             (narrow-to-region start end)
             (read (decode-coding-string
                    (buffer-string) 'utf-8-unix)))
      (delete-region (point-min) end))))

(defun holo-layer-epc-net-decode-length ()
  "Read a 24-bit hex-encoded integer from buffer."
  (string-to-number (buffer-substring-no-properties (point) (+ (point) 6)) 16))

(defun holo-layer-epc-prin1-to-string (sexp)
  "Like `prin1-to-string' but don't octal-escape non-ascii characters.
This is more compatible with the CL reader."
  (with-temp-buffer
    (let (print-escape-nonascii
          print-escape-newlines
          print-length
          print-level)
      (prin1 sexp (current-buffer))
      (buffer-string))))

(cl-defstruct holo-layer-epc-manager
  "Root object that holds all information related to an EPC activity.

`holo-layer-epc-start-epc' returns this object.

title          : instance name for displaying on the `holo-layer-epc-controller' UI
server-process : process object for the peer
commands       : a list of (prog . args)
port           : port number
connection     : holo-layer-epc-connection instance
methods        : alist of method (name . function)
sessions       : alist of session (id . deferred)
exit-hook      : functions for after shutdown EPC connection"
  title server-process commands port connection methods sessions exit-hooks)

(cl-defstruct holo-layer-epc-method
  "Object to hold serving method information.

name       : method name (symbol)   ex: 'test
task       : method function (function with one argument)
arg-specs  : arg-specs (one string) ex: \"(A B C D)\"
docstring  : docstring (one string) ex: \"A test function. Return sum of A,B,C and D\"
"
  name task docstring arg-specs)

(defvar holo-layer-epc-live-connections nil
  "[internal] A list of `holo-layer-epc-manager' objects.
those objects currently connect to the epc peer.
This variable is for debug purpose.")

(defun holo-layer-epc-server-process-name (uid)
  (format "holo-layer-epc-server:%s" uid))

(defun holo-layer-epc-server-buffer-name (uid)
  (format " *%s*" (holo-layer-epc-server-process-name uid)))

(defun holo-layer-epc-stop-epc (mngr)
  "Disconnect the connection for the server."
  (let* ((proc (holo-layer-epc-manager-server-process mngr))
         (buf (and proc (process-buffer proc))))
    (holo-layer-epc-disconnect (holo-layer-epc-manager-connection mngr))
    (when proc
      (accept-process-output proc 0 holo-layer-epc-accept-process-timeout t))
    (when (and proc (equal 'run (process-status proc)))
      (kill-process proc))
    (when buf  (kill-buffer buf))
    (setq holo-layer-epc-live-connections (delete mngr holo-layer-epc-live-connections))
    ))

(defun holo-layer-epc-args (args)
  "[internal] If ARGS is an atom, return it. If list, return the cadr of it."
  (cond
   ((atom args) args)
   (t (cadr args))))

(defun holo-layer-epc-init-epc-layer (mngr)
  "[internal] Connect to the server program and return an holo-layer-epc-connection instance."
  (let* ((mngr mngr)
         (conn (holo-layer-epc-manager-connection mngr))
         (channel (holo-layer-epc-connection-channel conn)))
    ;; dispatch incoming messages with the lexical scope
    (cl-loop for (method . body) in
             `((call
                . (lambda (args)
                    (holo-layer-epc-log "SIG CALL: %S" args)
                    (apply 'holo-layer-epc-handler-called-method ,mngr (holo-layer-epc-args args))))
               (return
                . (lambda (args)
                    (holo-layer-epc-log "SIG RET: %S" args)
                    (apply 'holo-layer-epc-handler-return ,mngr (holo-layer-epc-args args))))
               (return-error
                . (lambda (args)
                    (holo-layer-epc-log "SIG RET-ERROR: %S" args)
                    (apply 'holo-layer-epc-handler-return-error ,mngr (holo-layer-epc-args args))))
               (epc-error
                . (lambda (args)
                    (holo-layer-epc-log "SIG EPC-ERROR: %S" args)
                    (apply 'holo-layer-epc-handler-epc-error ,mngr (holo-layer-epc-args args))))
               (methods
                . (lambda (args)
                    (holo-layer-epc-log "SIG METHODS: %S" args)
                    (holo-layer-epc-handler-methods ,mngr (caadr args))))
               ) do
             (holo-layer-epc-signal-connect channel method body))
    (push mngr holo-layer-epc-live-connections)
    mngr))

(defun holo-layer-epc-manager-send (mngr method &rest messages)
  "[internal] low-level message sending."
  (let* ((conn (holo-layer-epc-manager-connection mngr)))
    (holo-layer-epc-net-send conn (cons method messages))))

(defun holo-layer-epc-manager-get-method (mngr method-name)
  "[internal] Return a method object. If not found, return nil."
  (cl-loop for i in (holo-layer-epc-manager-methods mngr)
           if (eq method-name (holo-layer-epc-method-name i))
           do (cl-return i)))

(defun holo-layer-epc-handler-methods (mngr uid)
  "[internal] Return a list of information for registered methods."
  (let ((info
         (cl-loop for i in (holo-layer-epc-manager-methods mngr)
                  collect
                  (list
                   (holo-layer-epc-method-name i)
                   (or (holo-layer-epc-method-arg-specs i) "")
                   (or (holo-layer-epc-method-docstring i) "")))))
    (holo-layer-epc-manager-send mngr 'return uid info)))

(defun holo-layer-epc-handler-called-method (mngr uid name args)
  "[internal] low-level message handler for peer's calling."
  (let ((mngr mngr) (uid uid))
    (let* ((_methods (holo-layer-epc-manager-methods mngr))
           (method (holo-layer-epc-manager-get-method mngr name)))
      (cond
       ((null method)
        (holo-layer-epc-log "ERR: No such method : %s" name)
        (holo-layer-epc-manager-send mngr 'epc-error uid (format "EPC-ERROR: No such method : %s" name)))
       (t
        (condition-case err
            (let* ((f (holo-layer-epc-method-task method))
                   (ret (apply f args)))
              (cond
               ((holo-layer-deferred-object-p ret)
                (holo-layer-deferred-nextc ret
                                          (lambda (xx) (holo-layer-epc-manager-send mngr 'return uid xx))))
               (t (holo-layer-epc-manager-send mngr 'return uid ret))))
          (error
           (holo-layer-epc-log "ERROR : %S" err)
           (holo-layer-epc-manager-send mngr 'return-error uid err))))))))

(defun holo-layer-epc-manager-remove-session (mngr uid)
  "[internal] Remove a session from the epc manager object."
  (cl-loop with ret = nil
           for pair in (holo-layer-epc-manager-sessions mngr)
           unless (eq uid (car pair))
           do (push pair ret)
           finally
           do (setf (holo-layer-epc-manager-sessions mngr) ret)))

(defun holo-layer-epc-handler-return (mngr uid args)
  "[internal] low-level message handler for normal returns."
  (let ((pair (assq uid (holo-layer-epc-manager-sessions mngr))))
    (cond
     (pair
      (holo-layer-epc-log "RET: id:%s [%S]" uid args)
      (holo-layer-epc-manager-remove-session mngr uid)
      (holo-layer-deferred-callback (cdr pair) args))
     (t                                 ; error
      (holo-layer-epc-log "RET: NOT FOUND: id:%s [%S]" uid args)))))

(defun holo-layer-epc-handler-return-error (mngr uid args)
  "[internal] low-level message handler for application errors."
  (let ((pair (assq uid (holo-layer-epc-manager-sessions mngr))))
    (cond
     (pair
      (holo-layer-epc-log "RET-ERR: id:%s [%S]" uid args)
      (holo-layer-epc-manager-remove-session mngr uid)
      (holo-layer-deferred-errorback (cdr pair) (format "%S" args)))
     (t                                 ; error
      (holo-layer-epc-log "RET-ERR: NOT FOUND: id:%s [%S]" uid args)))))

(defun holo-layer-epc-handler-epc-error (mngr uid args)
  "[internal] low-level message handler for epc errors."
  (let ((pair (assq uid (holo-layer-epc-manager-sessions mngr))))
    (cond
     (pair
      (holo-layer-epc-log "RET-EPC-ERR: id:%s [%S]" uid args)
      (holo-layer-epc-manager-remove-session mngr uid)
      (holo-layer-deferred-errorback (cdr pair) (list 'epc-error args)))
     (t                                 ; error
      (holo-layer-epc-log "RET-EPC-ERR: NOT FOUND: id:%s [%S]" uid args)))))

(defun holo-layer-epc-call-deferred (mngr method-name args)
  "Call peer's method with args asynchronously. Return a deferred
object which is called with the result."
  (let ((uid (holo-layer-epc-uid))
        (sessions (holo-layer-epc-manager-sessions mngr))
        (d (holo-layer-deferred-new)))
    (push (cons uid d) sessions)
    (setf (holo-layer-epc-manager-sessions mngr) sessions)
    (holo-layer-epc-manager-send mngr 'call uid method-name args)
    d))

(defun holo-layer-epc-define-method (mngr method-name task &optional arg-specs docstring)
  "Define a method and return a deferred object which is called by the peer."
  (let* ((method (make-holo-layer-epc-method
                  :name method-name :task task
                  :arg-specs arg-specs :docstring docstring))
         (methods (cons method (holo-layer-epc-manager-methods mngr))))
    (setf (holo-layer-epc-manager-methods mngr) methods)
    method))

(defun holo-layer-epc-sync (mngr d)
  "Wrap deferred methods with synchronous waiting, and return the result.
If an exception is occurred, this function throws the error."
  (let ((result 'holo-layer-epc-nothing))
    (holo-layer-deferred-chain
     d
     (holo-layer-deferred-nextc it
                               (lambda (x) (setq result x)))
     (holo-layer-deferred-error it
                               (lambda (er) (setq result (cons 'error er)))))
    (while (eq result 'holo-layer-epc-nothing)
      (save-current-buffer
        (accept-process-output
         (holo-layer-epc-connection-process (holo-layer-epc-manager-connection mngr))
         0 holo-layer-epc-accept-process-timeout t)))
    (if (and (consp result) (eq 'error (car result)))
        (error (cdr result)) result)))

(defun holo-layer-epc-call-sync (mngr method-name args)
  "Call peer's method with args synchronously and return the result.
If an exception is occurred, this function throws the error."
  (holo-layer-epc-sync mngr (holo-layer-epc-call-deferred mngr method-name args)))

(defun holo-layer-epc-live-p (mngr)
  "Return non-nil when MNGR is an EPC manager object with a live
connection."
  (let ((proc (ignore-errors
                (holo-layer-epc-connection-process (holo-layer-epc-manager-connection mngr)))))
    (and (processp proc)
         ;; Same as `process-live-p' in Emacs >= 24:
         (memq (process-status proc) '(run open listen connect stop)))))

;; epcs
(defvar holo-layer-epc-server-client-processes nil
  "[internal] A list of ([process object] . [`holo-layer-epc-manager' instance]).
When the server process accepts the client connection, the
`holo-layer-epc-manager' instance is created and stored in this variable
`holo-layer-epc-server-client-processes'. This variable is used for the management
purpose.")

;; holo-layer-epc-server
;;   name    : process name (string)   ex: "EPC Server 1"
;;   process : server process object
;;   port    : port number
;;   connect-function : initialize function for `holo-layer-epc-manager' instances
(cl-defstruct holo-layer-epc-server name process port connect-function)

(defvar holo-layer-epc-server-processes nil
  "[internal] A list of ([process object] . [`holo-layer-epc-server' instance]).
This variable is used for the management purpose.")

(defun holo-layer-epc-server-get-manager-by-process (proc)
  "[internal] Return the holo-layer-epc-manager instance for the PROC."
  (cl-loop for (pp . mngr) in holo-layer-epc-server-client-processes
           if (eql pp proc)
           do (cl-return mngr)
           finally return nil))

(defun holo-layer-epc-server-accept (process)
  "[internal] Initialize the process and return holo-layer-epc-manager object."
  (holo-layer-epc-log "HOLOLAYER-EPC-SERVER- >> Connection accept: %S" process)
  (let* ((connection-id (holo-layer-epc-uid))
         (connection-name (format "holo-layer-epc con %s" connection-id))
         (channel (list connection-name nil))
         (connection (make-holo-layer-epc-connection
                      :name connection-name
                      :process process
                      :buffer (process-buffer process)
                      :channel channel)))
    (holo-layer-epc-log "HOLOLAYER-EPC-SERVER- >> Connection establish")
    (set-process-coding-system process 'binary 'binary)
    (set-process-filter process
                        (lambda (p m)
                          (holo-layer-epc-process-filter connection p m)))
    (set-process-query-on-exit-flag process nil)
    (set-process-sentinel process
                          (lambda (p e)
                            (holo-layer-epc-process-sentinel connection p e)))
    (make-holo-layer-epc-manager :server-process process :port t
                                :connection connection)))

(defun holo-layer-epc-server-sentinel (process message connect-function)
  "[internal] Process sentinel handler for the server process."
  (holo-layer-epc-log "HOLOLAYER-EPC-SERVER- SENTINEL: %S %S" process message)
  (let ((mngr (holo-layer-epc-server-get-manager-by-process process)))
    (cond
     ;; new connection
     ((and (string-match "open" message) (null mngr))
      (condition-case err
          (let ((mngr (holo-layer-epc-server-accept process)))
            (push (cons process mngr) holo-layer-epc-server-client-processes)
            (holo-layer-epc-init-epc-layer mngr)
            (when connect-function (funcall connect-function mngr))
            mngr)
        ('error
         (holo-layer-epc-log "HOLOLAYER-EPC-SERVER- Protocol error: %S" err)
         (holo-layer-epc-log "HOLOLAYER-EPC-SERVER- ABORT %S" process)
         (delete-process process))))
     ;; ignore
     ((null mngr) nil )
     ;; disconnect
     (t
      (let ((pair (assq process holo-layer-epc-server-client-processes)) _d)
        (when pair
          (holo-layer-epc-log "HOLOLAYER-EPC-SERVER- DISCONNECT %S" process)
          (holo-layer-epc-stop-epc (cdr pair))
          (setq holo-layer-epc-server-client-processes
                (assq-delete-all process holo-layer-epc-server-client-processes))
          ))
      nil))))

(defun holo-layer-epc-server-start (connect-function &optional port)
  "Start TCP Server and return the main process object."
  (let*
      ((connect-function connect-function)
       (name (format "HOLO-LAYER EPC Server %s" (holo-layer-epc-uid)))
       (buf (holo-layer-epc-make-procbuf (format " *%s*" name)))
       (main-process
        (make-network-process
         :name name
         :buffer buf
         :family 'ipv4
         :server t
         :host "127.0.0.1"
         :service (or port t)
         :noquery t
         :sentinel
         (lambda (process message)
           (holo-layer-epc-server-sentinel process message connect-function)))))
    (push (cons main-process
                (make-holo-layer-epc-server
                 :name name :process main-process
                 :port (process-contact main-process :service)
                 :connect-function connect-function))
          holo-layer-epc-server-processes)
    main-process))

(provide 'holo-layer-epc)
;;; holo-layer-epc.el ends here
