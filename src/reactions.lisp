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

(defvar *current-reaction* nil)

;; All reaction operations are automatically synchronized
;;  by the means of reactors.

(defstruct (reaction
             (:include reactor-structure)
             (:conc-name %reaction-)
             (:predicate %reaction-p)
             (:constructor %make-reaction (function)))
"Instances of this structure class represent reactions
bound to slots of reactive objects."
  (invalidated nil)
  (value nil :type t)
  (function #'no-values :type function)
  (dependent (cons nil nil) :type (cons list list)))

(defmethod print-object ((object reaction) stream)
  (declare (type stream stream))
  (print-unreadable-object (object stream :type t :identity t)
    (pprint-logical-block (stream nil)
      (format stream "Reactor: ~s" (%reaction-reactor object))
      (pprint-newline :mandatory stream)))
  object)

(defun reaction-p (object)
  "Returns T if an object is a REACTION and NIL otherwise."
  (%reaction-p object))

(defun reaction-reactor (reaction)
  "Returns a reactor associated with this reaction object."
  (declare (type reaction reaction))
  (%reaction-reactor reaction))

(defun make-reaction (&optional (function #'no-values))
  "Constructs and initializes REACTION object."
  (declare (type function function))
  (let ((reaction (%make-reaction function)))
    (reaction-recompute reaction)
    reaction))

(defmacro reaction (&body body)
  "Expands to MAKE-REACTION."
  `(make-reaction (lambda () ,@body)))

(defun reaction-update-dependent (reaction)
  (declare (type reaction reaction))
  (let ((dependent (delete-duplicates (car (%reaction-dependent reaction))
                                      :test #'eq)))
    (setf (car (%reaction-dependent reaction)) nil
          (cdr (%reaction-dependent reaction)) nil)
    (dolist (d dependent)
      (declare (type reaction d))
      (reactor-invoke (%reaction-reactor d)
                      #'reaction-recompute
                      (list d)
                      :priority :reactions)))
  (values))

(defun reaction-recompute (reaction)
  (declare (type reaction reaction))
  (unless (%reaction-invalidated reaction)    
    (let* ((*current-reaction* reaction)
           (values (multiple-value-list
                     (funcall (%reaction-function reaction)))))
      (unless (null values)
        (setf (%reaction-value reaction) (car values)
              (%reaction-invalidated reaction) t)
        (reaction-update-dependent reaction)
        (setf (%reaction-invalidated reaction) nil))))
  (values))

(defun get-reaction-value (reaction current)
  (declare (type reaction reaction)
           (type (or null reaction) current))
  (when (and current (not (eq current reaction)))
    (let ((queue (%reaction-dependent reaction))
          (item (list current)))
      (if (null (car queue))
        (setf (car queue) item
              (cdr queue) item)
        (setf (cddr queue) item
              (cdr queue) item))))
  (%reaction-value reaction))

(defun reaction-value (reaction)
  (declare (type reaction reaction))
  (reactor-invoke (%reaction-reactor reaction)
                  #'get-reaction-value
                  (list reaction *current-reaction*)
                  :priority :reactions))

(defun set-reaction-value (reaction new-value)
  (declare (type reaction reaction))
  (setf (%reaction-value reaction) new-value
        (%reaction-invalidated reaction) t)
  (reaction-update-dependent reaction)
  (setf (%reaction-invalidated reaction) nil)
  new-value)

(defun (setf reaction-value) (new-value reaction)
  (declare (type reaction reaction))
  (reactor-invoke (%reaction-reactor reaction)
                  #'set-reaction-value
                  (list reaction new-value)
                  :priority :reactions))
