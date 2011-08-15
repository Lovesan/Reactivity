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

(in-package #:cl-user)

(defpackage #:reactivity
  (:use #:cl)
  #.`(:shadowing-import-from
       #:closer-mop ,@(loop :for sym :being :the :external-symbol
                        :in :closer-mop :collect (symbol-name sym)))
  (:export
    
    #:ensure-default-thread-bindings
    
    ;;reactor stuff
    #:reactor
    #:reactor-p
    #:current-reactor
    #:reactor-running-p
    #:reactor-priority
    #:reactor-run
    #:reactor-invoke
    #:reactor-shutdown
    
    ;;reactor operation stuff
    #:reactor-operation
    #:reactor-operation-p
    #:operation-reactor
    #:operation-values
    #:operation-function
    #:operation-args
    #:operation-completion
    #:operation-cancellation
    #:operation-priority
    #:operation-status
    #:operation-cancel
    #:operation-wait
    
    ;;conditions
    #:reactivity-error
    #:reactor-error
    #:reactor-error-reactor
    #:reaction-error
    #:reaction-error-reaction
    #:reaction-incompatible-reactor
    
    ;;reaction stuff
    #:reaction
    #:make-reaction
    #:reaction-p
    #:reaction-reactor
    
    ;;reactive class stuff
    #:reactive-class
    #:reactive-slot-definition
    #:reactive-slot-definition-reaction
    #:reactive-direct-slot-definition
    #:reactive-effective-slot-definition
    
    ;;reactive object stuff
    #:reactive-object
    #:reactive-object-reactor
    ))
