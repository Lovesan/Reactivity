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
        (dolist (thread (%operation-observers operation))
          (thread-notify thread))
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

(defun operation-execute (operation)
  (declare (type reactor-operation operation))
  (setf (%operation-status operation) :executing
        (%operation-values operation) (multiple-value-list
                                        (apply (%operation-function operation)
                                               (%operation-args operation)))
        (%operation-status operation) :completed)
  (funcall (%operation-completion operation))
  (with-reactor-lock ((%operation-reactor operation))
    (dolist (thread (%operation-observers operation))
      (thread-notify thread)))
  (values))

(defun operation-wait (operation)
"Waits for an operation to complete and returns values, returned
by the application of operation's function to its arguments.
In case when operation has been cancelled, returns zero values."
  (declare (type reactor-operation operation))
  (case (%operation-status operation)
    (:completed (values-list (%operation-values operation)))
    (:cancelled (values))
    (T (progn (let ((reactor (%operation-reactor operation)))
                (with-reactor-lock (reactor)
                  (push (current-thread-id) (%operation-observers operation)))
                (with-thread-loop
                    (loop
                      (let ((result (thread-wait-notification)))
                        (if (null result)
                          (let ((operation (reactor-dequeue reactor)))
                            (when operation (operation-execute operation)))
                          (let ((timer (gethash result (%reactor-timers reactor))))
                            (when timer
                              (reactor-invoke reactor
                                              (%timer-function timer)
                                              (%timer-args timer)
                                              :priority (%timer-priority timer)
                                              :wait nil)))))
                      (case (%operation-status operation)
                        (:completed (return (values-list (%operation-values operation))))
                        (:cancelled (return (values)))))))))))

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
:PRIORITY could be one of: :NORMAL(default), :REACTIONS, :RENDER, :INPUT or :IDLE."
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
    (thread-notify (%reactor-thread-id reactor))
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

(defun reactor-run ()
"Starts message loop on the current thread, utilizing
current thread's reactor."
  (with-thread-loop
      (let ((reactor (current-reactor)))
        (declare (type reactor reactor))
        (setf (%reactor-running reactor) t)
        (loop :while (%reactor-running reactor) :do
          (let ((result (thread-wait-notification)))
            (if (null result)
              (let ((operation (reactor-dequeue reactor)))
                (when operation (operation-execute operation)))
              (let ((timer (gethash result (%reactor-timers reactor))))
                (when timer
                  (reactor-invoke reactor
                                  (%timer-function timer)
                                  (%timer-args timer)
                                  :priority (%timer-priority timer)
                                  :wait nil)))))))))

(defun %timer-start (timer)
  (declare (type reactor-timer timer))
  (let* ((reactor (%timer-reactor timer))
         (timers (%reactor-timers reactor))
         (id (create-timer (%timer-interval timer))))
    (setf (%timer-id timer) id
          (gethash id timers) timer))
  (values))

(defun timer-start (timer)
"Starts the timer."
  (declare (type reactor-timer timer))
  (let ((reactor (%timer-reactor timer)))
    (with-reactor-lock (reactor)
      (when (%timer-running timer)
        (return-from timer-start nil))
      (setf (%timer-running timer) t))
    (reactor-invoke reactor
                    #'%timer-start
                    (list timer)
                    :wait nil)
    t))

(defun %timer-stop (timer)
  (declare (type reactor-timer timer))
  (let* ((reactor (%timer-reactor timer))
         (timers (%reactor-timers reactor))
         (id (%timer-id timer)))
    (remhash id timers)
    (timer-destroy id)
    (setf (%timer-id timer) 0))
  (values))

(defun timer-stop (timer)
"Stops the timer."
  (declare (type reactor-timer timer))
  (let ((reactor (%timer-reactor timer)))
    (with-reactor-lock (reactor)
      (unless (%timer-running timer)
        (return-from timer-stop nil))
      (setf (%timer-running timer) nil))
    (reactor-invoke reactor
                    #'%timer-stop
                    (list timer)
                    :wait nil)
    t))

(defun (setf timer-priority) (new-priority timer)
"Sets new priority level for the timer.
One of(from highest to lowest): :NORMAL, :REACTIONS, :RENDER, :INPUT or :IDLE."
  (declare (type reactor-priority new-priority)
           (type reactor-timer timer))
  (with-reactor-lock ((%timer-reactor timer))
    (setf (%timer-priority timer) new-priority)))

(defun (setf timer-function) (new-function timer)
"Sets new function for the timer.
This function is applied to timer's args, each time timer's interval elapses."
  (declare (type function new-function)
           (type reactor-timer timer))
  (with-reactor-lock ((%timer-reactor timer))
    (setf (%timer-function timer) new-function)))

(defun (setf timer-args) (new-args timer)
"Sets new argument list for timer's function."
  (declare (type list new-args)
           (type reactor-timer timer))
  (with-reactor-lock ((%timer-reactor timer))
    (setf (%timer-args timer) new-args)))

(defun %set-timer-interval (timer interval)
  (declare (type reactor-timer timer)
           (type unsigned-byte interval))
  (let* ((id (%timer-id timer))
         (present (gethash id (%reactor-timers
                                (%timer-reactor timer)))))
    (when present
      (timer-change-interval id interval)))
  (values))

(defun (setf timer-interval) (new-interval timer)
"Sets new interval for the timer.
Interval is measured in milliseconds."
  (declare (type unsigned-byte new-interval)
           (type reactor-timer timer))
  (block nil
    (with-reactor-lock ((%timer-reactor timer))
      (setf (%timer-interval timer) new-interval)
      (unless (%timer-running timer)
        (return new-interval)))
    (reactor-invoke (%timer-reactor timer)
                    #'%set-timer-interval
                    (list timer new-interval)
                    :wait nil)
    new-interval))

(defun make-timer (&key (reactor (current-reactor))
                        (interval 0)
                        (function #'no-values)
                        (args '())
                        (priority :normal)
                        (start nil))
"Constructs and returns a new instance of REACTOR-TIMER.
:REACTOR is a REACTOR that should be associated with this timer.
:INTERVAL - a non-negative integer. Interval is measured in milliseconds.
:FUNCTION - a function, that should be called each time timer elapses.
:ARGS - a list of arguments for the :FUNCTION.
:PRIORITY parameter represents timer's priority level,
One of: :NORMAL(default), :REACTIONS, :RENDER, :INPUT or :IDLE.
Unless :START equals to NIL, timer is started immediately."
  (declare (type unsigned-byte interval)
           (type reactor reactor)
           (type function function)
           (type list args)
           (type reactor-priority priority))
  (let ((timer (%make-timer
                 :reactor reactor
                 :interval interval
                 :function function
                 :args args
                 :priority priority)))
    (declare (type reactor-timer timer))
    (when start
      (timer-start timer))
    timer))
