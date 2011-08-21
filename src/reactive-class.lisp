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

(defclass reactive-class (standard-class)
  ()
  (:documentation "A metaclass for each reactive class."))

(defmethod validate-superclass ((class reactive-class)
                                (superclass standard-class))
  t)

(defmethod validate-superclass ((class standard-class)
                                (superclass reactive-class))
  t)

(defmethod ensure-class-using-class :after
    ((class reactive-class) name &key &allow-other-keys)
  (declare (ignore name))
  (finalize-inheritance class))

(defclass reactive-slot-definition (standard-slot-definition)
  ((%reaction
     :initform t
     :initarg :reaction
     :accessor reactive-slot-definition-reaction
     :documentation
     "Designated whether this slot is implemented as a REACTION.")))

(defclass reactive-direct-slot-definition
    (reactive-slot-definition standard-direct-slot-definition)
  ())

(defclass reactive-effective-slot-definition
    (reactive-slot-definition standard-effective-slot-definition)
  ())

(defmethod direct-slot-definition-class ((class reactive-class) &rest initargs)
  (declare (ignore initargs))
  (find-class 'reactive-direct-slot-definition))

(defmethod effective-slot-definition-class ((class reactive-class) &rest initargs)
  (declare (ignore initargs))
  (find-class 'reactive-effective-slot-definition))

(defmethod compute-effective-slot-definition ((class reactive-class) name slotds)
  (let ((slotd (find name slotds :key #'slot-definition-name))
        (object (call-next-method)))
    (setf (reactive-slot-definition-reaction object)
          (reactive-slot-definition-reaction slotd))
    object))
