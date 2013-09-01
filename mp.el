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
(require 'noflet)

(defun mp/parse-response (mp receiver proc data)
  (funcall mp :debug "mp/send %s < [%s]" data proc)
  (if (string-match "[\n]*\\(-[a-z]+\\) \\(.*\\)" data)
      (case (intern (concat ":" (match-string 1 data)))
        (:-emacs-pid t) ; no need to do anything - just an ack
        (:-print
         (let ((response
                (read   
                 (decode-coding-string
                  (server-unquote-arg (match-string 2 data))
                  'emacs-internal))))
           (funcall mp :debug "mp/send response %s" response)
           (if (eq :success (car response))
               (funcall receiver (cadr response) nil)
               ;; Else it's a failure
               (funcall receiver nil (car response))))))))

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
            (mp/parse-response mp receiver proc data))))
        (to-send
         (format
          "-eval %s\n"
          (server-quote-arg
           (format "%S" `(condition-case err
                             (list :success ,form)
                           (error err)))))))
    (when proc
      (funcall mp :debug "mp/send sending [%s] to {%s}" to-send proc)
      (process-send-string proc to-send))))

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
                 (debug t)
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
                       (:getdebug debug)
                       (:debug
                        (when debug
                          (message
                           (replace-regexp-in-string
                            "\n" "\\\\n"
                            (apply 'format other)))))
                       (:getsocket
                        (format "/tmp/emacs%d/%s"
                                (user-uid) unique))
                       (:kill (mp/send
                               this-func
                               '(kill-emacs)
                               (lambda (data error)
                                 (list data error))))
                       (:send
                        (destructuring-bind (form receiver) other
                          (mp/send this-func form receiver))))))))
      (setenv "HOME" saved-home))))


(progn
  ;; Sets up the mp remote error signal
  (put :mp-remote-error
       'error-conditions
       '(error :mp :mp-remote-error :remote-error))
  (put :mp-remote-error
       'error-message
       "a remote error occurred"))

(defmacro mp> (channel result-procname bodyform &rest nextform)
  "Execute BODYFORM on CHANNEL's process then execute NEXTFORM.

The BODYFORM is executed in another Emacs process.  When it
completes NEXTFORM is executed.  Within NEXTFORM a call to
RESULT-PROCNAME will return either the data resulting from
BODYFORM or raise a `:mp-remote-error'.  In this way NEXTFORM can
capture errors raised from the remote naturally.

NEXTFORM is wrapped in an implicit `condition-case' to respond to
`:mp-remote-error' whenever it is thrown, so a handler can defer to
the default handling of `:mp-remote-error'."
  (declare
   (debug (sexp sexp form &rest form))
   (indent 3))
  (let ((datav (make-symbol "data"))
        (errv (make-symbol "err"))
        (default-errv (make-symbol "defaulterrv")))
    `(funcall
      channel :send (quote ,bodyform)
      (lambda (,datav ,errv)
        (condition-case ,default-errv
            (nolexflet ((,result-procname ()
                          (if ,errv
                              (signal :mp-remote-error (list ,errv))
                              ,datav)))
              ,@nextform)
          (:mp-remote-error
           (funcall
            channel :debug
            "there was a remote error %s" ,default-errv)))))))

(defmacro with-mp (var &rest form)
  "Bind VAR a handle to another process and then eval FORM.

When FORM completes or exits, kill the process bound to VAR."
  (declare
   (debug (sexp &rest form))
   (indent 1))
  `(let ((,var (mp/start-daemon)))
     (unwind-protect
          (progn
            (sleep-for 5)
            ,@form)
       (funcall ,var :kill))))

(defun mp/testit ()
  "Crude test of the macros."
  (with-mp channel
    (mp> channel remote
        (progn (sleep-for 2) (* 10 15))
      (message "hurrah! %s" (remote)))
    (sleep-for 10)))

(defun mp/test-divide-by-zero ()
  (with-mp channel
    (mp> channel remote
        (progn (sleep-for 2) (/ 1 0))
      (condition-case err
          (message "hurrah! %s" (remote))
        (:mp-remote-error (message "whoops! divide by 0"))))
    (sleep-for 10)))

(provide 'mp)

;;; mp.el ends here
