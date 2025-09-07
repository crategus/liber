;;; ----------------------------------------------------------------------------
;;; generate-html.lisp
;;;
;;; Load this file from the Lisp prompt to generate the HTML documentation for
;;; the Liber library. The documentation is stored in the 'doc/' directory,
;;; which is a subdirectory of the root directory of the Liber library. If the
;;; directory does not exist, it will be created.
;;;
;;; Last updated: 2025-09-05
;;; ----------------------------------------------------------------------------

(asdf:load-system :liber)

;; Make HTML (single page)
(let* ((base (asdf:component-pathname (asdf:find-system :liber)))
       (output-directory (merge-pathnames "doc/single-page/" base)))
  (ensure-directories-exist output-directory)
  (liber:generate-html-documentation
      '(:liber)
      output-directory
      :index-title "Liber API documentation"
      :heading "Liber"
      :css "default.css"
      :single-page-p t
      :include-slot-definitions-p t
      :include-internal-symbols-p t))

;; Make HTML (multiple pages)
(let* ((base (asdf:component-pathname (asdf:find-system :liber)))
       (output-directory (merge-pathnames "doc/" base)))
  (ensure-directories-exist output-directory)
  (liber:generate-html-documentation
      '(:liber)
      output-directory
      :index-title "Liber API documentation"
      :heading "Liber"
      :css "default.css"
      :single-page-p nil
      :include-slot-definitions-p t
      :include-internal-symbols-p t))
