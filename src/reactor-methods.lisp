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

(defun operation-add (operation)
  (declare (type reactor-operation operation))
  (let ((item (list operation))
        (queue (svref (%reactor-queue
                        (%operation-reactor operation))
                      (%reactor-priority
                        (%operation-priority operation)))))
    (declare (type (cons list list) queue))
    (if (null (car queue))
      (setf (car queue) item
            (cdr queue) item)
      (setf (cddr queue) item
            (cdr queue) item))
    operation))

(defun operation-remove (operation)
  (declare (type reactor-operation operation))
  (let ((priority (%operation-priority operation))
        (reactor (%operation-reactor operation)))
    (let* ((queue (svref (%reactor-queue reactor)
                         (%reactor-priority priority))))
      (declare (type (cons list list) queue))
      (cond ((null (car queue))
             nil)
            ((eq (caar queue) operation)
             (when (null (setf (car queue) (cdar queue)))
               (setf (cdr queue) '()))
             operation)
            (T (loop :for iter = (car queue) :then (cdr iter) :do
                 (when (eq (cadr iter) operation)
                   (when (null (setf (cdr iter) (cddr iter)))
                     (setf (cdr queue) iter))
                   (return operation))))))))

(defun operation-cancel (operation)
"Revokes the operation, effectively removing it from it's reactor's
priority queue. Before this function exits, operation's cancellation
callback is called."
  (declare (type reactor-operation operation))
  (let ((reactor (%operation-reactor operation)))
    (with-reactor-lock (reactor)
      (unless (null (operation-remove operation))
        (setf (%operation-status operation) :cancelled)
        (funcall (%operation-cancellation operation))
        operation))))

(defun (setf operation-priority) (new-priority operation)
"Sets new priority level for an operation."
  (declare (type reactor-priority new-priority)
           (type reactor-operation operation))
  (let ((reactor (%operation-reactor operation)))
    (with-reactor-lock (reactor)      
      (if (null (operation-remove operation))
        (setf (%operation-priority operation) new-priority)
        (progn (setf (%operation-priority operation) new-priority)
               (operation-add operation)))
      new-priority)))

(defun reactor-dequeue (reactor)
  (declare (type reactor reactor))
  (with-reactor-lock (reactor)
    (let ((queue (%reactor-queue reactor)))      
      (dotimes (i (length queue))
        (let* ((queue (svref queue i))
               (item (pop (car queue))))
          (declare (type (cons list list) queue))
          (unless (null item)
            (when (null (car queue))
              (setf (cdr queue) '()))
            (let ((operation item))
              (declare (type reactor-operation operation))
              (return operation))))))))

(defun reactor-enqueue (reactor operation)
  (declare (type reactor reactor)
           (type reactor-operation operation))
  (with-reactor-lock (reactor)
    (operation-add operation)))

(defun operation-wait (operation)
"Waits for an operation to complete and returns values, returned
by the application of operation's function to its arguments.
In case when operation has been cancelled, returns zero values."
  (declare (type reactor-operation operation))
  (case (%operation-status operation)
    (:completed (values-list (%operation-values operation)))
    (:cancelled (values))
    (T (let ((reactor (%operation-reactor operation)))
         (if (eq reactor (current-reactor))
           (loop :for op = (reactor-dequeue reactor)
             :while op :do
             (setf (%operation-status op) :executing
                   (%operation-values op) (multiple-value-list
                                            (apply (%operation-function op)
                                                   (%operation-args op)))
                   (%operation-status op) :completed)
             (funcall (%operation-completion op))
             (when (eq op operation)
               (return (values-list (%operation-values operation)))))
           (let ((lock (%reactor-lock reactor))
                 (cvar (%reactor-cvar reactor)))
             (bt:acquire-lock lock)
             (loop (bt:condition-wait cvar lock)
               (case (%operation-status operation)
                 (:completed
                   (bt:release-lock lock)
                   (return (values-list (%operation-values operation))))
                 (:cancelled
                   (bt:release-lock lock)
                   (return (values)))))))))))

(defun reactor-invoke (reactor function args
                       &key (wait t)
                            (priority :normal)
                            (completion #'no-values)
                            (cancellation #'no-values))
"  Appends new operation(an application of FUNCTION to ARGS) into reactor's
priority queue.
  Unless :WAIT is NIL, operation is synchronous, otherwise this function
will return not the values returned by FUNCTION, but instead an object of type
REACTOR-OPERATION.
:COMPLETION callback will be called upon operation's completion.
:CANCELLATION routine will be called upon operation's cancellation.
:PRIORITY parameter represents operation's priority level,
:PRIORITY could be one of: :NORMAL, :BINDINGS, :RENDER, :INPUT or :IDLE."
  (declare (type reactor reactor)
           (type reactor-priority priority)
           (type function function completion cancellation)
           (type list args))
  (let ((operation (%make-operation
                     :reactor reactor
                     :function function
                     :args args
                     :priority priority
                     :completion completion
                     :cancellation cancellation)))
    (reactor-enqueue reactor operation)
    (notify-thread (%reactor-tid reactor))
    (if wait
      (operation-wait operation)
      operation)))

(defun reactor-shutdown (reactor &key (wait t)
                                      (completion #'no-values)
                                      (cancellation #'no-values))
"Temporary stops execution of reactor's message loop.
Unless :WAIT is NIL, operation is synchronous.
:COMPLETION callback will be called upon operation's completion.
:CANCELLATION routine will be called upon operation's cancellation."
  (declare (type reactor reactor)
           (type function completion cancellation))
  (reactor-invoke reactor
                  (lambda (reactor)
                    (declare (type reactor reactor))
                    (setf (%reactor-running reactor) nil)
                    (values))
                  (list reactor)
                  :wait wait
                  :completion completion
                  :cancellation cancellation))
