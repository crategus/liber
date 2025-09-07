(defpackage :blocks-world-system
  (:use :asdf :cl))

(in-package :blocks-world-system)

(defclass blocks-world-source-file (cl-source-file) ())

#+sbcl
(defmethod perform :around ((o compile-op) (s blocks-world-source-file))
  (handler-bind ((sb-ext:compiler-note #'muffle-warning))
    (let ((*compile-print* nil))
      (call-next-method))))

(asdf:defsystem :blocks-world
  :default-component-class blocks-world-source-file
  :serial t
  :components ((:file "package")
               (:file "blocks")
               (:file "goals"))
  :depends-on ())
