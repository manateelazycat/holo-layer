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

(defcustom holo-layer-active-window-color "#cc2444"
  "Border color for active window."
  :type 'string)

(defcustom holo-layer-inactive-window-color "#000000"
  "Border color for active window."
  :type 'string)

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
  (if (holo-layer-epc-live-p holo-layer-epc-process)
      (remove-hook 'post-command-hook #'holo-layer-start-process)
    ;; start epc server and set `holo-layer-server-port'
    (holo-layer--start-epc-server)
    (let* ((holo-layer-args (append
                             (list holo-layer-python-file)
                             (list (number-to-string holo-layer-server-port))
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
  (setq holo-layer-emacs-frame (window-frame))

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
  )

(defun holo-layer-emacs-running-in-wayland-native ()
  (eq window-system 'pgtk))

(defun holo-layer--get-titlebar-height ()
  "We need fetch height of window titlebar to adjust y coordinate of EAF when Emacs is not fullscreen."
  (cond ((holo-layer-emacs-running-in-wayland-native)
         (let ((is-fullscreen-p (memq (frame-parameter nil 'fullscreen) '(fullscreen fullboth))))
           (if is-fullscreen-p
               0
             ;; `32' is titlebar of Gnome3, we need change this value in other environment.
             (cond ((string-equal (getenv "XDG_CURRENT_DESKTOP") "Hyprland")
                    0)
                   (t
                    32)))))
        (t
         0)))

(defvar holo-layer-build-dir (file-name-directory (locate-library "holo-layer")))

(defun holo-layer--get-frame-coordinate ()
  "We need fetch Emacs coordinate to adjust coordinate of EAF if it running on system not support cross-process reparent technology.

Such as, wayland native, macOS etc."
  (cond ((string-equal (getenv "XDG_CURRENT_DESKTOP") "sway")
         (holo-layer--split-number (shell-command-to-string (concat holo-layer-build-dir "swaymsg-treefetch/swaymsg-rectfetcher.sh emacs"))))
        ((string-equal (getenv "XDG_CURRENT_DESKTOP") "Hyprland")
         (let ((clients (json-parse-string (shell-command-to-string "hyprctl -j clients")))
               (coordinate))
           (dotimes (i (length clients))
             (when (equal (gethash "pid" (aref clients i)) (emacs-pid))
               (setq coordinate (gethash "at" (aref clients i)))))
           (list (aref coordinate 0) (aref coordinate 1))))
        ((holo-layer-emacs-running-in-wayland-native)
         (require 'dbus)
         (let* ((coordinate (holo-layer--split-number
                             (dbus-call-method :session "org.gnome.Shell" "/org/eaf/wayland" "org.eaf.wayland" "get_emacs_window_coordinate" :timeout 1000)
                             ","))
                ;; HiDPI need except by `frame-scale-factor'.
                (frame-x (truncate (/ (car coordinate) (frame-scale-factor))))
                (frame-y (truncate (/ (cadr coordinate) (frame-scale-factor)))))
           (list frame-x frame-y)))
        (t
         (list 0 0))))

(defun holo-layer-get-window-allocation (&optional window)
  "Get WINDOW allocation."
  (let* ((window-edges (window-pixel-edges window))
         (x (nth 0 window-edges))
         (y (+ (nth 1 window-edges)
               (if (version< emacs-version "27.0")
                   (window-header-line-height window)
                 (window-tab-line-height window))))
         (w (- (nth 2 window-edges) x))
         (h (- (nth 3 window-edges) (window-mode-line-height window) y)))
    (list x y w h)))

(defun holo-layer--split-number (string)
  (mapcar #'string-to-number (split-string string)))

(defun holo-layer--frame-left (frame)
  "Return outer left position"
  (let ((left (frame-parameter frame 'left)))
    (if (listp left) (nth 1 left) left)))

(defun holo-layer--frame-top (frame)
  "Return outer top position."
  (let ((top (frame-parameter frame 'top)))
    (if (listp top) (nth 1 top) top)))

(defun holo-layer--frame-internal-height (frame)
  "Height of internal objects.
Including title-bar, menu-bar, offset depends on window system, and border."
  (let ((geometry (frame-geometry frame)))
    (+ (cdr (alist-get 'title-bar-size geometry))
       (cdr (alist-get 'tool-bar-size geometry)))))

(defun holo-layer-is-normal-window-p (window)
  (not (or (minibufferp (window-buffer window))
           (and (require 'sort-tab nil t)
                (string-equal (buffer-name (window-buffer window)) sort-tab-buffer-name)))))

(defun holo-layer-get-emacs-frame-info ()
  (let ((pos (frame-position))
        (width (frame-pixel-width))
        (height (frame-pixel-height))
        (external-border-size (cdr (nth 2 (frame-geometry))))
        (title-bar-size (or (cdr (nth 4 (frame-geometry)))
                            (cons 0 0))))
    (list (+ (car pos) (car external-border-size) (car title-bar-size))
          (+ (cdr pos) (cdr external-border-size) (cdr title-bar-size))
          width
          height)))

(defun holo-layer-eaf-fullscreen-p ()
  (and (require 'eaf nil t)
       eaf-fullscreen-p
       (equal (length (cl-remove-if #'window-dedicated-p (window-list frame))) 1)))

(defvar holo-layer-emacs-is-focus-p t
  "Whether Emacs is currently focused.")

(defun holo-layer-focus-in-hook-function ()
  (setq holo-layer-emacs-is-focus-p t)
  (holo-layer-monitor-configuration-change))

(defun holo-layer-focus-out-hook-function ()
  (setq holo-layer-emacs-is-focus-p nil)
  (holo-layer-monitor-configuration-change))

(defun holo-layer-monitor-configuration-change (&rest _)
  "EAF function to respond when detecting a window configuration change."
  (when (and (holo-layer-epc-live-p holo-layer-epc-process)
             ;; When current frame is same with `emacs-frame'.
             (equal (window-frame) holo-layer-emacs-frame))
    (ignore-errors
      (let ((emacs-frame-info (holo-layer-get-emacs-frame-info))
            (current-window (selected-window))
            view-infos)
        (cond
         ;; Support EAF fullscreen.
         ((or (not holo-layer-emacs-is-focus-p)
              (holo-layer-eaf-fullscreen-p))
          (holo-layer-call-async "update_window_info" emacs-frame-info ""))
         ;; Support blink-search.
         ((and (require 'blink-search nil t)
               (equal (buffer-name (window-buffer current-window)) blink-search-input-buffer))
          (let* ((top-window (get-buffer-window blink-search-start-buffer))
                 (top-window-info (holo-layer-get-window-info holo-layer-emacs-frame top-window current-window))
                 (top-window-info-list (split-string top-window-info ":")))
            (push top-window-info view-infos)
            (push (format "%s:%s:%s:%s:%s"
                          (nth 0 top-window-info-list)
                          (+ (string-to-number (nth 1 top-window-info-list))
                             (string-to-number (nth 3 top-window-info-list)))
                          (nth 2 top-window-info-list)
                          (+ (window-pixel-height (get-buffer-window blink-search-input-buffer))
                             (window-pixel-height (get-buffer-window blink-search-candidate-buffer)))
                          (equal current-window current-window))
                  view-infos)
            (holo-layer-call-async "update_window_info" emacs-frame-info (mapconcat #'identity view-infos ","))))
         ;; Normal window layout.
         (t
          (dolist (frame (frame-list))
            (dolist (window (window-list frame))
              (when (and (equal (window-frame window) holo-layer-emacs-frame)
                         (holo-layer-is-normal-window-p window))
                (push (holo-layer-get-window-info frame window current-window) view-infos))))
          (holo-layer-call-async "update_window_info" emacs-frame-info (mapconcat #'identity view-infos ",")))
         )))))

(defun holo-layer-get-window-info (frame window current-window)
  (with-current-buffer (window-buffer window)
    (let* ((window-allocation (holo-layer-get-window-allocation window))
           (window-divider-right-padding (if window-divider-mode window-divider-default-right-width 0))
           (window-divider-bottom-padding (if window-divider-mode window-divider-default-bottom-width 0))
           (titlebar-height (holo-layer--get-titlebar-height))
           (frame-coordinate (holo-layer--get-frame-coordinate))
           (frame-x (car frame-coordinate))
           (frame-y (cadr frame-coordinate))
           (x (nth 0 window-allocation))
           (y (nth 1 window-allocation))
           (w (nth 2 window-allocation))
           (h (nth 3 window-allocation)))
      (format "%s:%s:%s:%s:%s"
              (+ x frame-x)
              (+ y titlebar-height frame-y)
              (- w window-divider-right-padding)
              (- h window-divider-bottom-padding)
              (equal window current-window)))))

(defun holo-layer-enable ()
  (add-hook 'post-command-hook #'holo-layer-start-process)

  (add-hook 'window-size-change-functions #'holo-layer-monitor-configuration-change)
  (add-hook 'window-configuration-change-hook #'holo-layer-monitor-configuration-change)
  (add-hook 'buffer-list-update-hook #'holo-layer-monitor-configuration-change)

  (add-hook 'focus-in-hook 'holo-layer-focus-in-hook-function)
  (add-hook 'focus-out-hook 'holo-layer-focus-out-hook-function)

  (setq-default mode-line-format nil))

(defun holo-layer-disable ()
  (remove-hook 'post-command-hook #'holo-layer-start-process)

  (remove-hook 'window-size-change-functions #'holo-layer-monitor-configuration-change)
  (remove-hook 'window-configuration-change-hook #'holo-layer-monitor-configuration-change)
  (remove-hook 'buffer-list-update-hook #'holo-layer-monitor-configuration-change)

  (remove-hook 'focus-in-hook 'holo-layer-focus-in-hook-function)
  (remove-hook 'focus-out-hook 'holo-layer-focus-out-hook-function)

  ;; hide holo layer
  (holo-layer-call-async "update_window_info" (holo-layer-get-emacs-frame-info) ""))

(unless holo-layer-is-starting
  (holo-layer-start-process))

(provide 'holo-layer)

;;; holo-layer.el ends here
