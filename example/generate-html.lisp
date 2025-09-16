;;; ----------------------------------------------------------------------------
;;; example/generate-html.lisp
;;;
;;; Load this file from the Lisp prompt to generate the HTML documentation for
;;; the blocks-world examle. The documentation is stored in the 'example/doc/'
;;; directory, which is a subdirectory of the root directory of the Liber
;;; library. If the directory does not exist, it will be created.
;;;
;;; Last updated: 2025-09-15
;;; ----------------------------------------------------------------------------

(asdf:load-system :liber/generate)
(asdf:load-system :blocks-world)

;; Make HTML (multiple pages)
(let* ((base (asdf:component-pathname (asdf:find-system :blocks-world)))
       (output (merge-pathnames "doc/" base)))
  (liber:generate-html-documentation
      '(:blocks-world :blocks-world-goals)
      base
      output
      :author "Crategus"
      :author-url "http://www.crategus.com"
      :index-title "Blocks World API documentation"
      :heading "blocks-world"
      :css "default.css"
      :single-page-p nil
      :include-slot-definitions-p t
      :include-internal-symbols-p nil
      :delete-tmp-files-p t
      :verbose t))

;; Make HTML (single page)
(let* ((base (asdf:component-pathname (asdf:find-system :blocks-world)))
       (output (merge-pathnames "doc/single-page/" base)))
  (liber:generate-html-documentation
      '(:blocks-world :blocks-world-goals)
      base
      output
      :author "Crategus"
      :author-url "http://www.crategus.com"
      :index-title "Blocks World API documentation"
      :heading "blocks-world"
      :css "default.css"
      :icon "lambda.icon"
      :single-page-p t
      :include-slot-definitions-p t
      :include-internal-symbols-p nil
      :delete-tmp-files-p t
      :verbose t))
