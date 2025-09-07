(asdf:load-system :liber)
(asdf:load-system :blocks-world)

(defpackage :blocks-world-documentation
  (:use :common-lisp)
  (:export #:generate-html
           #:generate-html-single-page))

(in-package :blocks-world-documentation)

;; Make HTML (multiple pages)
(defun generate-html ()
  (let* ((base (asdf:component-pathname (asdf:find-system :blocks-world)))
         (output-directory (merge-pathnames "doc/" base)))
    (ensure-directories-exist output-directory)
    (liber:generate-html-documentation
      '(:blocks-world :blocks-world-goals)
      output-directory
      :author "Crategus"
      :author-url "http://www.crategus.com"
      :index-title "Blocks World API documentation"
      :heading "blocks-world"
      :css "default.css"
      :ico "lambda.icon"
      :single-page-p nil
      :paginate-section-p nil
      :include-slot-definitions-p t
      :include-internal-symbols-p nil)))

(defun generate-html-single-page ()
  (let* ((base (asdf:component-pathname (asdf:find-system :blocks-world)))
         (output-directory (merge-pathnames "doc/single-page/" base)))
    (ensure-directories-exist output-directory)
    (liber:generate-html-documentation
      '(:blocks-world :blocks-world-goals)
      output-directory
      :author "Crategus"
      :author-url "http://www.crategus.com"
      :index-title "Blocks World API documentation"
      :heading "blocks-world"
      :css "default.css"
      :ico "lambda.icon"
      :single-page-p t
      :include-slot-definitions-p t
      :include-internal-symbols-p nil)))

(generate-html)
(generate-html-single-page)
