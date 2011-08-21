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

(defclass reactive-object ()
  ((%reactor :initform (current-reactor) :reaction nil
             :reader reactive-object-reactor
             :documentation "A reactor associated with an instance of REACTIVE-OBJECT."))
  (:metaclass reactive-class)
  (:documentation "Every class with reactive slots inherit from this class."))

(finalize-inheritance (find-class 'reactive-object))

(defmethod compute-class-precedence-list ((class reactive-class))
  (let ((cpl (call-next-method)))
    (if (find-if (lambda (c)
                   (subtypep (class-name c) 'reactive-object))
                 cpl)
      cpl
      (append cpl (list (find-class 'reactive-object))))))

(defvar *slot-direct-access* nil)

(defmethod slot-value-using-class
    ((class reactive-class) (object reactive-object)
     (slotd reactive-slot-definition))
  (let ((value (call-next-method)))
    (if (or *slot-direct-access*
            (not (reactive-slot-definition-reaction slotd)))
      value
      (reaction-value (the reaction value)))))

(defun %reactive-object-reactor (object)
  (declare (type reactive-object object))
  (let ((*slot-direct-access* t))
    (if (slot-boundp object '%reactor)
      (reactive-object-reactor object)
      (setf (slot-value object '%reactor)
            (current-reactor)))))

(defmethod (setf slot-value-using-class)
    (new-value (class reactive-class) (object reactive-object)
               (slotd reactive-slot-definition))
  (if (or *slot-direct-access*
          (not (reactive-slot-definition-reaction slotd)))
    (call-next-method)
    (if (%reaction-p new-value)
      (let ((reactor (%reactive-object-reactor object)))
        (if (eq reactor (%reaction-reactor new-value))
          (with-reactor-lock (reactor)
            (call-next-method))
          (error 'reaction-incompatible-reactor
                 :reaction new-value
                 :reactor reactor)))
      (let ((reaction (let ((*slot-direct-access* t))
                        (if (slot-boundp-using-class class object slotd)
                          (slot-value-using-class class object slotd)
                          (setf (slot-value-using-class class object slotd)
                                (make-reaction))))))
        (setf (reaction-value reaction) new-value)))))
