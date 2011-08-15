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

(deftype thread () 'doors:dword)

(defun thread= (thread1 thread2)
  (declare (type thread thread1 thread2))
  (= thread1 thread2))

(defun current-thread ()
  "Returns current native Windows thread identifier."
  (virgil:external-function-call
    "GetCurrentThreadId"
    ((:stdcall doors:kernel32)
     (doors:dword))))

(defun thread-notify (thread)
"Notifies native Windows thread with message loop about
the new pending operation."
  (virgil:external-function-call
    #+doors.unicode "PostThreadMessageW"
    #-doors.unicode "PostThreadMessageA"
    ((:stdcall doors:user32)
     ((doors:last-error virgil:boolean))
     (doors:dword)
     (virgil:uint)
     (doors:wparam)
     (doors:lparam))
    thread
    (1+ #x8000) ;; WM_APP+1
    0
    0))

(defun thread-wait-notification ()
  "Dispatches Windows messages on the current thread until WP_APP+1 is spotted."
  (let ((msg (doors.ui:make-msg)))
    (declare (dynamic-extent msg))
    (loop (doors.ui:get-message msg)
      (doors.ui:translate-message msg)
      (doors.ui:dispatch-message msg)
      (when (= (1+ #x8000) (doors.ui:msg-message msg))
        (return (values))))))

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
