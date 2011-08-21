;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-

;;; Copyright (C) 2011, Dmitry Ignatiev <lovesan.ru at gmail.com>

;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use, copy,
;;; modify, merge, publish, distribute, sublicense, and/or sell copies
;;; of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:

;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.

;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT.  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE

(in-package #:reactivity)

(deftype thread-id () 'doors:dword)
(deftype timer-id () 'doors:ulong-ptr)

(defun current-thread-id ()
  "Returns current native Windows thread identifier."
  (virgil:external-function-call
    "GetCurrentThreadId"
    ((:stdcall doors:kernel32)
     (doors:dword))))

(defun thread-notify (thread-id)
"Notifies native Windows thread with message loop about
the new pending operation."
  (declare (type thread-id thread-id))
  (virgil:external-function-call
    #+doors.unicode "PostThreadMessageW"
    #-doors.unicode "PostThreadMessageA"
    ((:stdcall doors:user32)
     ((doors:last-error virgil:boolean))
     (doors:dword)
     (virgil:uint)
     (doors:wparam)
     (doors:lparam))
    thread-id
    (1+ #x00008000) ;; WM_APP+1
    0
    0))

(declaim (inline not-zero))
(defun not-zero (x)
  (/= x 0))

(defun create-timer (interval)
"Creates a timer associated with current native Windows thread.
INTERVAL is measured in milliseconds."
  (declare (type unsigned-byte interval))
  (let ((interval (logand interval #xFFFFFFFF)))
    (declare (type doors:dword interval))
    (virgil:external-function-call "SetTimer"
      ((:stdcall doors:user32)
       ((doors:last-error doors:ulong-ptr not-zero))
       (doors:handle hwnd :aux nil)
       (doors:ulong-ptr id :aux 0)
       (virgil:uint interval :aux interval)
       (doors:ulong-ptr fn :aux 0)))))

(defun timer-change-interval (timer-id interval)
"Changes interval of a timer associated with current native Windows thread.
INTERVAL is measured in milliseconds."
  (declare (type timer-id timer-id)
           (type unsigned-byte interval))
  (let ((interval (logand interval #xFFFFFFFF)))
    (declare (type doors:dword interval))
    (virgil:external-function-call "SetTimer"
      ((:stdcall doors:user32)
       ((doors:last-error doors:ulong-ptr not-zero))
       (doors:handle hwnd :aux nil)
       (doors:ulong-ptr id :aux timer-id)
       (virgil:uint interval :aux interval)
       (doors:ulong-ptr fn :aux 0)))))

(defun timer-destroy (timer-id)
"Destroys a timer associated with current native Windows thread."
  (declare (type timer-id timer-id))
  (virgil:external-function-call "KillTimer"
    ((:stdcall doors:user32)
     ((doors:last-error virgil:boolean))
     (doors:handle hwnd :aux nil)
     (doors:ulong-ptr id :aux timer-id))))

(defun thread-wait-notification ()
"Dispatches Windows messages on the current thread until WP_APP+1
or WM_TIMER is spotted. In case of WM_APP+1, returns NIL, otherwise
returns timer identifier"
  (let ((msg (doors.ui:make-msg)))
    (declare (dynamic-extent msg))
    (loop (doors.ui:get-message msg)
      (doors.ui:translate-message msg)
      (doors.ui:dispatch-message msg)
      ;; WM_APP+1
      (when (= (1+ #x00008000) (doors.ui:msg-message msg))
        (return nil))
      ;; WM_TIMER(wParam is timer identifier)
      (when (and (= #x00000113 (doors.ui:msg-message msg))
                 (zerop (doors.ui:msg-lparam msg)))
        (return (doors.ui:msg-wparam msg))))))

(defmacro with-thread-loop (&body body)
"During message loop execution COM is initialized to STA,
for reasons of interoperability with OLE(e.g. drag'n'drop and so on)."
  `(progn
     (handler-bind ((warning #'muffle-warning))
       (virgil:external-function-call
         "CoInitialize"
         ((:stdcall doors:ole32)
          (doors:hresult)
          (doors:handle))
         nil))
     (unwind-protect
         (progn ,@body)
       (virgil:external-function-call
         "CoUninitialize"
         ((:stdcall doors:ole32)
          (virgil:void))))))
