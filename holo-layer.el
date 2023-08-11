;;; holo-layer.el --- LSP bridge  -*- lexical-binding: t -*-

;; Filename: holo-layer.el
;; Description: LSP bridge
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2018, Andy Stewart, all rights reserved.
;; Created: 2018-06-15 14:10:12
;; Version: 0.5
;; Last-Updated: 2022-10-10 15:23:53 +0800
;;           By: Gong Qijian
;; URL: https://github.com/manateelazycat/holo-layer
;; Keywords:
;; Compatibility: emacs-version >= 28
;; Package-Requires: ((emacs "28") (posframe "1.1.7") (markdown-mode "2.6"))
;;
;; Features that might be required by this library:
;;
;; Please check README
;;

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Holo-Layer
;;

;;; Installation:
;;
;; Please check README
;;

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET holo-layer RET
;;

;;; Change log:
;;
;;

;;; Acknowledgements:
;;
;;
;;

;;; TODO
;;
;;
;;

;;; Code:
(require 'cl-lib)
(require 'json)
(require 'map)
(require 'seq)
(require 'subr-x)
(require 'holo-layer-epc)

(defgroup holo-layer nil
  "Holo-Layer group."
  :group 'applications)

(defvar holo-layer-server nil
  "The Holo-Layer Server.")

(defvar holo-layer-python-file (expand-file-name "holo_layer.py" (if load-file-name
                                                                           (file-name-directory load-file-name)
                                                                         default-directory)))

(defvar holo-layer-server-port nil)

(defun holo-layer--start-epc-server ()
  "Function to start the EPC server."
  (unless (process-live-p holo-layer-server)
    (setq holo-layer-server
          (holo-layer-epc-server-start
           (lambda (mngr)
             (let ((mngr mngr))
               (holo-layer-epc-define-method mngr 'eval-in-emacs 'holo-layer--eval-in-emacs-func)
               (holo-layer-epc-define-method mngr 'get-emacs-var 'holo-layer--get-emacs-var-func)
               (holo-layer-epc-define-method mngr 'get-emacs-vars 'holo-layer--get-emacs-vars-func)
               (holo-layer-epc-define-method mngr 'get-user-emacs-directory 'holo-layer--user-emacs-directory)
               ))))
    (if holo-layer-server
        (setq holo-layer-server-port (process-contact holo-layer-server :service))
      (error "[Holo-Layer] holo-layer-server failed to start")))
  holo-layer-server)

(defun holo-layer--eval-in-emacs-func (sexp-string)
  (eval (read sexp-string))
  ;; Return nil to avoid epc error `Got too many arguments in the reply'.
  nil)

(defun holo-layer--get-emacs-var-func (var-name)
  (let* ((var-symbol (intern var-name))
         (var-value (symbol-value var-symbol))
         ;; We need convert result of booleanp to string.
         ;; Otherwise, python-epc will convert all `nil' to [] at Python side.
         (var-is-bool (prin1-to-string (booleanp var-value))))
    (list var-value var-is-bool)))

(defun holo-layer--get-emacs-vars-func (&rest vars)
  (mapcar #'holo-layer--get-emacs-var-func vars))

(defvar holo-layer-epc-process nil)

(defvar holo-layer-internal-process nil)
(defvar holo-layer-internal-process-prog nil)
(defvar holo-layer-internal-process-args nil)

(defcustom holo-layer-name "*holo-layer*"
  "Name of Holo-Layer buffer."
  :type 'string)

(defcustom holo-layer-python-command (if (memq system-type '(cygwin windows-nt ms-dos)) "python.exe" "python3")
  "The Python interpreter used to run holo_layer.py."
  :type 'string)

(defcustom holo-layer-enable-debug nil
  "If you got segfault error, please turn this option.
Then Holo-Layer will start by gdb, please send new issue with `*holo-layer*' buffer content when next crash."
  :type 'boolean)

(defcustom holo-layer-enable-log nil
  "Enable this option to print log message in `*holo-layer*' buffer, default only print message header."
  :type 'boolean)

(defcustom holo-layer-enable-profile nil
  "Enable this option to output performance data to ~/holo-layer.prof."
  :type 'boolean)

(defun holo-layer--user-emacs-directory ()
  "Get lang server with project path, file path or file extension."
  (expand-file-name user-emacs-directory))

(defun holo-layer-call-async (method &rest args)
  "Call Python EPC function METHOD and ARGS asynchronously."
  (if (holo-layer-epc-live-p holo-layer-epc-process)
      (holo-layer-deferred-chain
       (holo-layer-epc-call-deferred holo-layer-epc-process (read method) args))
    (setq holo-layer-first-call-method method)
    (setq holo-layer-first-call-args args)
    (holo-layer-start-process)))

(defvar holo-layer-is-starting nil)
(defvar holo-layer-first-call-method nil)
(defvar holo-layer-first-call-args nil)

(defun holo-layer-restart-process ()
  "Stop and restart Holo-Layer process."
  (interactive)
  (setq holo-layer-is-starting nil)

  (holo-layer-kill-process)
  (holo-layer-start-process)
  (message "[Holo-Layer] Process restarted."))

(defun holo-layer-start-process ()
  "Start Holo-Layer process if it isn't started."
  (setq holo-layer-is-starting t)
  (unless (holo-layer-epc-live-p holo-layer-epc-process)
    ;; start epc server and set `holo-layer-server-port'
    (holo-layer--start-epc-server)
    (let* ((holo-layer-args (append
                                (list holo-layer-python-file)
                                (list (number-to-string holo-layer-server-port))
                                (when holo-layer-enable-profile
                                  (list "profile"))
                                )))

      ;; Set process arguments.
      (if holo-layer-enable-debug
          (progn
            (setq holo-layer-internal-process-prog "gdb")
            (setq holo-layer-internal-process-args (append (list "-batch" "-ex" "run" "-ex" "bt" "--args" holo-layer-python-command) holo-layer-args)))
        (setq holo-layer-internal-process-prog holo-layer-python-command)
        (setq holo-layer-internal-process-args holo-layer-args))

      ;; Start python process.
      (let ((process-connection-type t))
        (setq holo-layer-internal-process
              (apply 'start-process
                     holo-layer-name holo-layer-name
                     holo-layer-internal-process-prog holo-layer-internal-process-args)))
      (set-process-query-on-exit-flag holo-layer-internal-process nil))))

(defvar holo-layer-stop-process-hook nil)

(defun holo-layer-kill-process ()
  "Stop Holo-Layer process and kill all Holo-Layer buffers."
  (interactive)

  ;; Run stop process hooks.
  (run-hooks 'holo-layer-stop-process-hook)

  ;; Kill process after kill buffer, make application can save session data.
  (holo-layer--kill-python-process))

(add-hook 'kill-emacs-hook #'holo-layer-kill-process)

(defun holo-layer--kill-python-process ()
  "Kill Holo-Layer background python process."
  (when (holo-layer-epc-live-p holo-layer-epc-process)
    ;; Cleanup before exit Holo-Layer server process.
    (holo-layer-call-async "cleanup")
    ;; Delete Holo-Layer server process.
    (holo-layer-epc-stop-epc holo-layer-epc-process)
    ;; Kill *holo-layer* buffer.
    (when (get-buffer holo-layer-name)
      (kill-buffer holo-layer-name))
    (setq holo-layer-epc-process nil)
    (message "[Holo-Layer] Process terminated.")))

(defun holo-layer--first-start (holo-layer-epc-port)
  "Call `holo-layer--open-internal' upon receiving `start_finish' signal from server."
  ;; Make EPC process.
  (setq holo-layer-epc-process (make-holo-layer-epc-manager
                                   :server-process holo-layer-internal-process
                                   :commands (cons holo-layer-internal-process-prog holo-layer-internal-process-args)
                                   :title (mapconcat 'identity (cons holo-layer-internal-process-prog holo-layer-internal-process-args) " ")
                                   :port holo-layer-epc-port
                                   :connection (holo-layer-epc-connect "127.0.0.1" holo-layer-epc-port)
                                   ))
  (holo-layer-epc-init-epc-layer holo-layer-epc-process)
  (setq holo-layer-is-starting nil)

  (when (and holo-layer-first-call-method
             holo-layer-first-call-args)
    (holo-layer-deferred-chain
     (holo-layer-epc-call-deferred holo-layer-epc-process
                                      (read holo-layer-first-call-method)
                                      holo-layer-first-call-args)
     (setq holo-layer-first-call-method nil)
     (setq holo-layer-first-call-args nil)
     ))

  (message "*******"))

(unless holo-layer-is-starting
  (holo-layer-start-process))

(provide 'holo-layer)

;;; holo-layer.el ends here
