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

(defun reactor-run ()
"Starts Windows message loop on the current thread, utilizing
current thread's reactor.
During message loop execution, COM is initialized to STA,
for reasons of interoperability with OLE(e.g. drag'n'drop and so on)."
  (virgil:external-function-call
    "CoInitialize"
    ((:stdcall doors:ole32)
     (doors:hresult)
     (doors:handle))
    nil)
  (unwind-protect
      (let ((reactor (current-reactor))
            (message (doors.ui:make-msg)))
        (setf (%reactor-running reactor) t)
        (loop :while (%reactor-running reactor) :do
          (doors.ui:get-message message)
          (when (= (1+ #x8000) (doors.ui:msg-message message))
            (loop :for op = (reactor-dequeue reactor)
              :while op :do
              (setf (%operation-status op) :executing
                    (%operation-values op) (multiple-value-list
                                             (apply (%operation-function op)
                                                    (%operation-args op)))
                    (%operation-status op) :completed)
              (funcall (%operation-completion op))
              (bt:condition-notify (%reactor-cvar reactor))))
          (doors.ui:translate-message message)
          (doors.ui:dispatch-message message))
        (values))
    (virgil:external-function-call
      "CoUninitialize"
      ((:stdcall doors:ole32)
       (virgil:void)))))
