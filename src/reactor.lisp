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

;; Priority levels, highest->lowest: normal, reactions, render, input, idle
;;
;; should i add some more in the future?

(defvar *current-reactor* nil)

(defstruct (reactor
             (:constructor %make-reactor ())
             (:predicate %reactor-p)
             (:conc-name %reactor-))
"Reactor represents an event dispatcher associated with
certain thread. An instance of this structure class could be
obtained by CURRENT-REACTOR function."
  (thread-id (current-thread-id) :type thread-id)
  (lock (bt:make-lock))
  (queue (map-into (make-array 5) (lambda () (cons nil nil)))
         :type (simple-array T (5)))
  (timers (make-hash-table :test #'eql) :type hash-table)
  (running nil))

(defun reactor-p (object)
"Returns T if an object is a REACTOR and NIL otherwise."
  (%reactor-p object))

(defmethod print-object ((object reactor) stream)
  (declare (type stream stream))
  (print-unreadable-object (object stream :type t :identity t)
    (pprint-logical-block (stream nil)
      (format stream "Running: ~:[No~;Yes~]"
              (%reactor-running object))
      (pprint-newline :mandatory stream)
      (format stream "Thread ID: ~s" (%reactor-thread-id object))
      (pprint-newline :mandatory stream)))
  object)

(defun reactor-running-p (reactor)
  "Returns T if the reactor is already running and NIL otherwise."
  (declare (type reactor reactor))
  (%reactor-running reactor))

(defmacro with-reactor-lock ((reactor) &body body)
  (let ((var (gensym)))
    `(let ((,var ,reactor))
       (declare (type reactor ,var))
       (bt:with-lock-held ((%reactor-lock ,var))
         ,@body))))

(deftype reactor-priority ()
"Each reactor operation has a priority.
One of(from highest to lowest): :NORMAL, :REACTIONS, :RENDER, :INPUT or :IDLE."
  '(member :normal :reactions :render :input :idle))

(defun %reactor-priority (priority)
  (ecase priority
    (:normal 0)
    (:reactions 1)
    (:render 2)
    (:input 3)
    (:idle 4)))

(defun ensure-default-thread-bindings ()
"Each BT:THREAD must start with *CURRENT-REACTOR* variable initialized to NIL."
  (bt:start-multiprocessing)
  (setf bt:*default-special-bindings*
        (cons '(*current-reactor* . nil)
              (remove '*current-reactor*
                      bt:*default-special-bindings*
                      :key #'car))))

(defun current-reactor ()
"Returns a reactor object for current thread.
Unless thread's reactor already exists, it is created."
  (if *current-reactor*
    (let ((thread-id (current-thread-id)))
      (unless (= thread-id (%reactor-thread-id *current-reactor*))
        ;; reactor is out of date (e.g. lisp image has been restarted)
        (setf (%reactor-thread-id *current-reactor*) thread-id
              (%reactor-lock *current-reactor*) (bt:make-lock))
        (maphash (lambda (k v)
                   (declare (ignore k))
                   (setf (%timer-running v) nil
                         (%timer-id v) 0))
          (%reactor-timers *current-reactor*))
        (clrhash (%reactor-timers *current-reactor*)))
      *current-reactor*)
    (setf *current-reactor* (%make-reactor))))

(defun no-values (&rest args)
  (declare (ignore args))
  (values))

(defstruct (reactor-structure
             (:constructor nil)
             (:conc-name %reactor-structure-)
             (:predicate %reactor-structure-p))
  (reactor (current-reactor) :type reactor))

(defun reactor-structure-p (object)
"Returns T if an object is a REACTOR-STRUCTURE and NIL otherwise."  
  (%reactor-structure-p object))

(defun reactor-structure-reactor (reactor-structure)
"Returns a reactor that created this REACTOR-STRUCTURE."
  (declare (type reactor-structure reactor-structure))
  (%reactor-structure-reactor reactor-structure))

(deftype operation-status ()
"Each reactor operation has a status.
One of: :PENDING, :EXECUTING, :COMPLETED or :CANCELLED."
  '(member :pending :executing :completed :cancelled))

(defstruct (reactor-operation
             (:include reactor-structure)
             (:conc-name %operation-)
             (:predicate %operation-p)
             (:constructor %make-operation))
"Instances of this structure class represent
pieces of code that must be executed by the reactor."
  (values '() :type list)
  (function #'no-values :type function)
  (args '() :type list)
  (observers '() :type list)
  (completion #'no-values :type function)
  (cancellation #'no-values :type function)
  (priority :normal :type reactor-priority)
  (status :pending :type operation-status))

(defmethod print-object ((object reactor-operation) stream)
  (declare (type stream stream))
  (print-unreadable-object (object stream :type t :identity t)
    (pprint-logical-block (stream nil)
      (format stream "Reactor: ~s" (%operation-reactor object))
      (pprint-newline :mandatory stream)
      (format stream "Priority: ~:(~a~)" (%operation-priority object))
      (pprint-newline :mandatory stream)
      (format stream "Status: ~:(~a~)" (%operation-status object))
      (pprint-newline :mandatory stream)
      (when (%operation-observers object)
        (format stream "Observers: ~{~s~^, ~}" (%operation-observers object))
        (pprint-newline :mandatory stream))
      (when (eq :completed (%operation-status object))
        (format stream "Values: ~{~s~^, ~}" (%operation-values object))
        (pprint-newline :mandatory stream))))
  object)

(defun reactor-operation-p (object)
"Returns T if an object is an instance of REACTOR-OPERATION and NIL otherwise."
  (%operation-p object))

(defun operation-reactor (operation)
"Returns a reactor that created an instance of this REACTOR-OPERATION."  
  (declare (type reactor-operation operation))
  (%operation-reactor operation))

(defun operation-values (operation)
"Returns multiple values that have been returned by operation's function.
Unless function has completed its execution, returns zero values."
  (declare (type reactor-operation operation))
  (values-list (%operation-values operation)))

(defun operation-function (operation)
"Returns a function association with this operation."
  (declare (type reactor-operation operation))
  (%operation-function operation))

(defun operation-args (operation)
"Returns arguments, associated with operation's function."
  (declare (type reactor-operation operation))
  (copy-list (%operation-args operation)))

(defun operation-completion (operation)
"Returns callback that should be executed upon operation's function completion."
  (declare (type reactor-operation operation))
  (%operation-completion operation))

(defun (setf operation-completion) (new-completion operation)
"Sets new completion callback for an operation."
  (declare (type reactor-operation operation)
           (type function new-completion))
  (setf (%operation-completion operation) new-completion))

(defun operation-cancellation (operation)
"Returns callback that should be executed upon operations's cancellation."
  (declare (type reactor-operation operation))
  (%operation-cancellation operation))

(defun (setf operation-cancellation) (new-cancellation operation)
"Sets new cancellation callback for an operation."
  (declare (type reactor-operation operation)
           (type function new-cancellation))
  (setf (%operation-cancellation operation) new-cancellation))

(defun operation-priority (operation)
"Returns current priority of an operation."
  (declare (type reactor-operation operation))
  (%operation-priority operation))

(defun operation-status (operation)
"Returns current status of an operation."
  (declare (type reactor-operation operation))
  (%operation-status operation))

(defstruct (reactor-timer
             (:include reactor-structure)
             (:conc-name %timer-)
             (:constructor %make-timer)
             (:predicate %timer-p))
"Represents a timer associated with certain reactor."
  (id 0 :type timer-id)
  (running nil)
  (interval 0 :type unsigned-byte)
  (function #'no-values :type function)
  (args '() :type list)
  (priority :normal :type reactor-priority))

(defmethod print-object ((object reactor-timer) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (pprint-logical-block (stream nil)
      (format stream "Reactor: ~s" (%timer-reactor object))
      (pprint-newline :mandatory stream)
      (format stream "Running: ~:[No~;Yes~]" (%timer-running object))
      (pprint-newline :mandatory stream)
      (format stream "Priority: ~:(~a~)" (%timer-priority object))
      (pprint-newline :mandatory stream)
      (format stream "Interval: ~Dms" (%timer-interval object))
      (pprint-newline :mandatory stream)))
  object)

(defun reactor-timer-p (object)
"Returns T if an object is an instance of REACTOR-TIMER and NIL otherwise."
  (%timer-p object))

(defun timer-reactor (timer)
"Returns a REACTOR associated with the timer."
  (declare (type reactor-timer timer))
  (%timer-reactor timer))

(defun timer-interval (timer)
"Returns an interval at which timer is elapsed.
Interval is measured in milliseconds."
  (declare (type reactor-timer timer))
  (%timer-interval timer))

(defun timer-function (timer)
"Returns a function associated with the timer."
  (declare (type reactor-timer timer))
  (%timer-function timer))

(defun timer-args (timer)
"Returns a list of arguments associated with timer's function."
  (declare (type reactor-timer timer))
  (%timer-args timer))

(defun timer-priority (timer)
"Returns timer's priority.
One of(from highest to lowest): :NORMAL, :REACTIONS, :RENDER, :INPUT or :IDLE."
  (declare (type reactor-timer timer))
  (%timer-priority timer))

(defun timer-running-p (timer)
"Returns T if timer is already started and NIL otherwise."
  (declare (type reactor-timer timer))
  (%timer-running timer))

(ensure-default-thread-bindings)
