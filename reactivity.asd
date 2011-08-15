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

(eval-when (:compile-toplevel :load-toplevel :execute)
  #+allegro (progn #+mswindows (pushnew :windows *features*)
                   #+macosx (pushnew :darwin *features*)
                   #+(or macosx darwin freebsd netbsd openbsd)
                   (pushnew :bsd *features*))
  #+clisp (progn #+win32 (pushnew :windows *features*)
                 #-win32
                 (pushnew (with-standard-io-syntax
                              (read-from-string
                                (format nil ":~(~A~)"
                                        (posix:uname-sysname (posix:uname)))))
                          *features*)
                 #+(or darwin freebsd netbsd openbsd)
                 (pushnew :bsd *features*))
  #+cormanlisp (pushnew :windows *features*)
  #+ecl (progn #+darwin (pushnew :unix *features*)
               #+win32 (pushnew :windows *features*))
  #+lispworks (progn #+win32 (pushnew :windows *features*)
                     #+(or darwin freebsd netbsd openbsd)
                     (pushnew :bsd *features*))
  #+openmcl (progn #+linux-target (pushnew :linux *features*)
                   #+darwin (pushnew :bsd *features*))
  #+sbcl (progn #+win32
                (progn
                  ;; note: as of 2008 or so, SBCL doesn't push :UNIX and :WIN32
                  ;; simultaneously anymore.
                  (setq *features* (remove :unix *features*))
                  (pushnew :windows *features*))))

(defsystem #:reactivity
  :version "0.0.2"
  :description "Reactivity - a reactive GUI framework for Common Lisp."
  :author "Dmitry Ignatiev <lovesan.ru at gmail.com>"
  :maintainer "Dmitry Ignatiev <lovesan.ru at gmail.com>"
  :licence "MIT"
  :depends-on (#:bordeaux-threads
               #:closer-mop
               #:virgil
               #+windows #:doors)
  :serial t
  :components ((:module "src"
                        :serial t
                        :components ((:file "packages")
                                     (:file "conditions")
                                     #+windows
                                     (:file "threading-windows")
                                     #-windows
                                     (:file "threading-pthreads")
                                     (:file "reactor")
                                     (:file "reactor-methods")
                                     (:file "reactions")
                                     (:file "reactive-class")
                                     (:file "reactive-object")))))

;;;; vim: ft=lisp et
