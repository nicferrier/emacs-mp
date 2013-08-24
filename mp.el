;;; mp.el --- multiprocess emacs -*- lexical-binding: t -*-

;; Copyright (C) 2013  Nic Ferrier

;; Author: Nic Ferrier <nferrier@ferrier.me.uk>
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

;; This is code to help you start anonymous daemon processes and talk
;; to them asynchronously.

;; Right now we are only capable of using UNIX daemon sockets (as used
;; by the emacsclient protocol) to start the server.  In the future it
;; may be possible to use TCP/IP sockets.

;;; Code:

(require 'server) ;; for sending data to the daemon

(defun mp/send (mp form receiver)
  "Async send FORM to daemon MP.

When the call completes call RECEIVER with the result.  RECEIVER
should take two arguments, RESULT and ERROR.  `nil' is passed for
the argument that is not relevant."
  ;; More error handling here? FIXME - We need an error PROCESS DIED
  (let ((proc
         (make-network-process
          :name "mp/sender"
          :family 'local
          :service 0
          :remote (funcall mp :getsocket)
          :filter
          (lambda (proc data)
            (when (funcall mp :getdebug)
              (message "mp/send %s < [%s]" data proc))
            (if (string-match "[\n]*\\(-[a-z]+\\) \\(.*\\)" data)
                (case (intern (concat ":" (match-string 1 data)))
                  (:-emacs-pid t) ; no need to do anything - just an ack
                  (:-print
                   (funcall
                    receiver
                    (read
                     (decode-coding-string
                      (server-unquote-arg (match-string 2 data))
                      'emacs-internal)) nil)))))))
        (data (format
               "-eval %s\n"
               (server-quote-arg (format "%S" form)))))
    (when proc
      (when (funcall mp :getdebug)
        (message "sending [%s] to {%s}" data proc))
      (process-send-string proc data))))

(defun mp/start-daemon  ()
  "Start an Emacs server process."
  (let* ((unique (make-temp-name "mp-emacsd"))
         (emacs-dir (concat "/tmp/" unique))
         (emacs-bin (concat invocation-directory invocation-name))
         (args (list (concat "--daemon=" unique)))
         (saved-home (getenv "HOME")))
    (unwind-protect
         (progn
           (setenv "HOME" emacs-dir)
           (make-directory emacs-dir t)
           (let ((this-proc
                  (apply 'start-process
                         unique (format "*%s*" unique)
                         emacs-bin args))
                 (state :starting)
                 this-func)
             (set-process-sentinel
              this-proc (lambda (process state)
                          (cond
                            ((equal state "finished\n")
                             (setq state :live))
                            (t
                             (message "daemon state %s" state)))))
             (setq this-func
                   (lambda (msg &rest other)
                     (case msg
                       (:getdir emacs-dir)
                       (:getdebug t)
                       (:getsocket
                        (format "/tmp/emacs%d/%s"
                                (user-uid) unique))
                       (:send
                        (destructuring-bind (form receiver) other
                          (mp/send this-func form receiver))))))))
      (setenv "HOME" saved-home))))

(setq mptest (mp/start-daemon))
(funcall mptest :send
         '(progn (sleep-for 2) (* 10 15))
         (lambda (data error)
           (if error
               (message "meh: %s" error)
               (message "hurrah! %s" data))))

(provide 'mp)

;;; mp.el ends here
