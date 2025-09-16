;;; ----------------------------------------------------------------------------
;;; liber.asd
;;;
;;; Copyright (c) 2006 - 2008 David Lichteblau
;;; Copyright (c) 2012 - 2025 Dieter Kaiser
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a
;;; copy of this software and associated documentation files (the "Software"),
;;; to deal in the Software without restriction, including without limitation
;;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;;; and/or sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included in
;;; all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
;;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.
;;; ----------------------------------------------------------------------------

(defpackage :liber-system
  (:use :asdf :common-lisp))

(in-package :liber-system)

(defclass liber-source-file (cl-source-file) ())

#+sbcl
(defmethod perform :around ((o compile-op) (s liber-source-file))
  (handler-bind ((sb-ext:compiler-note #'muffle-warning))
    (let ((*compile-print* nil))
      (call-next-method))))

(defsystem :liber
  :name "liber"
  :version "0.1.0"
  :author "Dieter Kaiser"
  :license "MIT"
  :default-component-class liber-source-file
  :components
  ((:module src
    :serial t
    :components
    ((:file "package")
     (:file "liber-generate")
     (:file "liber-application"))))
  :depends-on (:liber/generate
               :cl-cffi-gtk4))

(defsystem :liber/generate
  :name "liber/generate"
  :version "0.1.0"
  :author "Dieter Kaiser"
  :license "MIT"
  :components
  ((:module src
    :serial t
    :components
    ((:file "package")
     (:file "liber-generate"))))
  :depends-on (:cxml
               :xuriella
               :closer-mop
               :split-sequence
               :iterate))

(defsystem :liber/test
  :name "liber/test"
  :version "0.1.0"
  :author "Dieter Kaiser"
  :license "MIT"
  :components
  ((:module test
    :serial t
    :components
    ((:file "test"))))
  :depends-on (:liber/generate))

;;; --- End of file liber.asd --------------------------------------------------
