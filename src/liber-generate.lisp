;;; ----------------------------------------------------------------------------
;;; liber.lisp
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

(in-package :liber)

(defvar *verbose* nil)
(defvar *stat*)

(defstruct statistic
  (external-symbols 0 :type integer)
  (internal-symbols 0 :type integer)
  (variables nil)
  (macros nil)
  (operators nil)
  (generics nil)
  (functions nil)
  (classes nil)
  (types nil)
  (symbols nil))

(defun sum-of-symbols (instance)
  (+ (length (statistic-variables instance))
     (length (statistic-macros instance))
     (length (statistic-operators instance))
     (length (statistic-generics instance))
     (length (statistic-functions instance))
     (length (statistic-classes instance))
     (length (statistic-types instance))
     (length (statistic-symbols instance))))

(defun lookup (key &optional (instance *stat*))
  (cond ((eq key :variables)
         (statistic-variables instance))
        ((eq key :macros)
         (statistic-macros instance))
        ((eq key :operators)
         (statistic-operators instance))
        ((eq key :generics)
         (statistic-generics instance))
        ((eq key :functions)
         (statistic-functions instance))
        ((eq key :classes)
         (statistic-classes instance))
        ((eq key :types)
         (statistic-types instance))
        ((eq key :symbols)
         (statistic-symbols instance))
        (t
         (format t "LOOKUP: Unknown key ~a~%" key))))

;;; ----------------------------------------------------------------------------

(let ((external-symbols (make-hash-table)))
  ;; Sets documentation string for external symbol
  (defun (setf symbol-documentation) (docstring symbol)
    (setf (gethash symbol external-symbols) docstring))

  ;; Gets documentation string for external symbol
  (defun symbol-documentation (symbol)
   "@version{2025-09-04}
    @syntax{(liber:symbol-documentation symbol) => docstring}
    @syntax{(setf (liber-symbol-documentation symbol) docstring)}
    @argument[symbol]{an external symbol for the package}
    @argument[docstring]{a string for the documentation string}
    @begin{short}
      Gets or sets the documentation string for an external symbol.
    @end{short}"
    (gethash symbol external-symbols))

  ;; Returns list with documentation strings for all external symbols
  (defun list-external-symbols ()
    "List of all documentation strings for external symbols."
    (iter (for (key value) in-hashtable external-symbols)
          (collect (list key value)))))

;;; ----------------------------------------------------------------------------

(defvar *symbol-name-alias* (make-hash-table)
  "Hash table to store the alias for the name of a symbol.")

(defun alias-for-symbol (name)
  "Gets the alias for the NAME of a symbol."
  (gethash name *symbol-name-alias*))

(defun (setf alias-for-symbol) (alias name)
  "Sets the ALIAS for the NAME of a symbol."
  (setf (gethash name *symbol-name-alias*) alias))

(defun list-symbol-name-alias ()
  "List of all aliases for the name of the symbols found in the package."
  (iter (for (key value) in-hashtable *symbol-name-alias*)
        (collect value)))

;;; ----------------------------------------------------------------------------

(defvar *function-name-alias* (make-hash-table))

(defun alias-for-function (name)
  (gethash name *function-name-alias*))

(defun (setf alias-for-function) (alias name)
  (setf (gethash name *function-name-alias*) alias))

;;; ----------------------------------------------------------------------------

(defvar *class-name-alias* (make-hash-table))

(defun alias-for-class (name)
  (gethash name *class-name-alias*))

(defun (setf alias-for-class) (alias name)
  (setf (gethash name *class-name-alias*) alias))

;;; ----------------------------------------------------------------------------

(defvar *variable-name-alias* (make-hash-table))

(defun alias-for-variable (name)
  (gethash name *variable-name-alias*))

(defun (setf alias-for-variable) (alias name)
  (setf (gethash name *variable-name-alias*) alias))

;;; ----------------------------------------------------------------------------

(defvar *type-name-alias* (make-hash-table))

(defun alias-for-type (name)
  (gethash name *type-name-alias*))

(defun (setf alias-for-type) (alias name)
  (setf (gethash name *type-name-alias*) alias))

;;; ----------------------------------------------------------------------------

(defun get-date ()
  "Gets the actual date in the format yyyy-mm-dd as a string."
  (multiple-value-bind (second minute hour date month year day-of-week dst-p tz)
      (get-decoded-time)
    (declare (ignore second minute hour day-of-week dst-p tz))
    (format nil "~d-~2,'0d-~2,'0d" year month date)))

#+sbcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require 'sb-introspect))

;;; ----------------------------------------------------------------------------

;; This code has been taken from a docstring extractor for the sbcl manual
;; to simplify the lambda list of a function for the documentation.

#+sbcl
(defun lambda-list (doc)
  (labels ((clean (x &key optional key)
    (typecase x
      (atom x)
      ((cons (member &optional))
       (cons (car x) (clean (cdr x) :optional t)))
      ((cons (member &key))
       (cons (car x) (clean (cdr x) :key t)))
      ((cons (member &whole &environment))
       ;; Skip these
       (clean (cdr x) :optional optional :key key))
      ((cons cons)
       (cons
         (cond (key (if (consp (caar x))
                        (caaar x)
                        (caar x)))
               (optional (caar x))
               (t (clean (car x))))
         (clean (cdr x) :key key :optional optional)))
      (cons
        (cons (cond ((or key optional) (car x))
                    (t (clean (car x))))
              (clean (cdr x) :key key :optional optional))))))
    (clean (sb-introspect:function-lambda-list doc))))

#-sbcl
(defun lambda-list (fun)
  (swank::arglist fun))

;;; ----------------------------------------------------------------------------

(defun copy-file (a b &key (if-exists :error))
  (with-open-file (in a :element-type '(unsigned-byte 8))
    (with-open-file (out b :direction :output
                           :if-exists if-exists
                           :element-type '(unsigned-byte 8))
      (let ((buf (make-array #x2000 :element-type '(unsigned-byte 8))))
        (iter (for pos = (read-sequence buf in))
              (until (zerop pos))
              (write-sequence buf out :end pos))))))

;;; ----------------------------------------------------------------------------

;; Make a special namestring from filenname:
;;   e.g. "crategus.css" => "/home/dieter/github/liber/css/crategus.css"
;;   but  ".liber.xml"   => ".liber.xml"
(defun magic-namestring (filename)
  (let ((directory (asdf:component-pathname (asdf:find-system :liber))))
    (unless (and (stringp filename) (char= (char filename 0) #\.))
      (let* ((kind (pathname-type filename))
             (base (merge-pathnames (format nil "~a/" kind) directory)))
        (setf filename (merge-pathnames filename base))))
    (namestring filename)))

;;; ----------------------------------------------------------------------------

;; Get the documentation string of a slot. There is no Common Lisp function to
;; access the documentation string.
;; Note: This is tested for SBCL. It does not work for a Condition Type on SBCL.
;; There seems to be no way to get the documenation string for a slot of
;; Condition Type in SBCL.

(defun slot-documentation (slot-name class-name)
  (let ((pos (position (string-upcase slot-name)
                       (mapcar #'closer-mop:slot-definition-name
                               (closer-mop:class-direct-slots
                                   (find-class class-name)))
                       :test 'string=)))
    (nth pos (closer-mop:class-direct-slots (find-class class-name)))))

;;; ----------------------------------------------------------------------------

(defvar *stylesheets*)

(defun flush-cache ()
  (setf *stylesheets* (make-hash-table :test 'equal)))

(flush-cache)

(defun grovel-stylesheet-dependencies (namestring)
  (let ((dependencies '()))
    (klacks:with-open-source (source (cxml:make-source (pathname namestring)))
      (iter (for event = (klacks:peek-next source))
            (while event)
            (when (and (eq event :start-element)
                       (string= (klacks:current-uri source)
                                "http://www.w3.org/1999/XSL/Transform")
                       (or (string= (klacks:current-lname source) "import")
                           (string= (klacks:current-lname source) "include")))
              (push (make-pathname :type "xsl"
                                   :defaults
                                   (merge-pathnames
                                       (klacks:get-attribute source "href")
                                       namestring))
                    dependencies))))
    dependencies))

(defun find-stylesheet (namestring)
  (let ((cache-entry (gethash namestring *stylesheets*)))
    (unless (and cache-entry
                 (every (lambda (dependency)
                          (eql (file-write-date (car dependency))
                               (cdr dependency)))
                        (cdr cache-entry)))
      (when *verbose*
        (if cache-entry
            (format t "~&Stylesheet has changed, reloading : ~A~%" namestring)
            (format t "~&Loading stylesheet : ~A~%" namestring)))
      (let ((dependencies (grovel-stylesheet-dependencies namestring)))
        (dolist (dependency dependencies)
          (with-open-file (s (make-pathname :type "tmp" :defaults dependency)
                             :direction :output
                             :if-exists :rename-and-delete)
            (xuriella:apply-stylesheet
                (pathname (magic-namestring "macros.xsl"))
                dependency
                :output s)))
        (setf cache-entry
              (cons (xuriella:parse-stylesheet
                      (xuriella:apply-stylesheet
                          (pathname (magic-namestring "macros.xsl"))
                          (pathname namestring)))
                    (mapcar (lambda (file)
                              (cons file (file-write-date file)))
                            (list* namestring
                                   (magic-namestring "macros.xsl")
                                   dependencies))))
        (setf (gethash namestring *stylesheets*)
              cache-entry)))
    (car cache-entry)))

(defun apply-stylesheet-chain (input stylesheets output)
  (let ((input (pathname (magic-namestring input)))
        (output (pathname (magic-namestring output))))
    (iter (for input-designator first input then result)
          (for (stylesheet . rest) on stylesheets)
          (for output-designator = (if rest nil output))
          (for result = (xuriella:apply-stylesheet
                            (find-stylesheet (magic-namestring stylesheet))
                            input-designator
                            :output output-designator)))))

;;; ----------------------------------------------------------------------------

(defvar *include-slot-definitions-p* nil)
(defvar *include-internal-symbols-p* nil)

(defun extract-documentation (packages directory-out
                                       &rest keys
                                       &key include-slot-definitions-p
                                            include-internal-symbols-p
                                       &allow-other-keys)
 "@version{2025-09-15}
  @argument[packages]{list of package designators, documentation will be
    generated for these packages}
  @argument[directory-out]{a pathname specifying a directory, the output file
    will be written to this directory, which must already exist}
  @argument[include-slot-definitions-p]{a boolean}
  @argument[keys]{extra parameters for stylesheets}
  @begin{short}
    Extracts docstrings from @arg{packages} and writes them in XML syntax to
    @file{.liber.xml} in the specified directory.
  @end{short}
  With @arg{include-slot-definitions-p}, class documentation will include a list
  of direct slots.

  Extra parameters will be inserted as attributes on the root element.
  @see-function{liber:generate-html-documentation}"
  (setf packages (mapcar #'find-package packages))
  (let ((*include-slot-definitions-p* include-slot-definitions-p)
        (*include-internal-symbols-p* include-internal-symbols-p))
    (with-open-file (stream (merge-pathnames ".liber.xml" directory-out)
                            :element-type '(unsigned-byte 8)
                            :direction :output
                            :if-does-not-exist :create
                            :if-exists :rename-and-delete)
      (cxml:with-xml-output (cxml:make-octet-stream-sink stream)
        (cxml:with-element "documentation"
          (iter (for (key value) on keys :by #'cddr)
                (when value
                  (let ((attr (format nil "~a" (string-downcase key))))
                    (cxml:attribute attr value))))
          (dolist (package packages)
            (let ((*package* package))
              (when *verbose*
                (format t "Extract documentation : ~a~%" package))
              (emit-package package packages))))))))

(defun generate-html-documentation
       (packages base output &key (author nil)
                                  (author-url nil)
                                  (date (get-date))
                                  (index-title "No Title")
                                  (heading "No Heading")
                                  (css "default.css")
                                  (icon "lambda.icon")
                                  (single-page-p nil)
                                  (paginate-section-p nil)
                                  (include-slot-definitions-p nil)
                                  (include-internal-symbols-p nil)
                                  (delete-tmp-files-p t)
                                  (verbose nil))
 "@version{2025-09-12}
  @argument[packages]{list of package designators, documentation will be
    generated for these packages}
  @argument[output]{a pathname specifying a directory, all output files and
    temporary data will be written to this directory, if the directory does not
    exist, it will be created}
  @argument[index-title]{a string for the title of the main page,
    @file{index.html}, other pages will be named according to the object they
    are documenting}
  @argument[heading]{a string for a visible title on top of every page}
  @argument[css]{a pathname or namestring pointing to a cascading stylesheet
    (CSS) file, this file will be copied to the target directory under the name
    @file{index.css}, if this argument is a string and does not start with a
    dot, it will be taken as namestring relative to the @file{liber/css}
    directory}
  @argument[single-page-p]{a boolean}
  @argument[include-slot-definitions-p]{a boolean}
  @argument[include-internal-symbols-p]{a boolean}
  @return{The pathname of the generated file @file{index.xml}.}
  @begin{short}
    Generates HTML documentation for @code{packages}.
  @end{short}
  With @arg{single-page-p}, all documentation is assembled as a single page
  called @file{index.html}. Otherwise, @file{index.html} will include only a
  symbol index and a summary of each package, with links to other pages.

  With @arg{include-slot-definitions-p}, pages for symbols that are not exported
  will be included, so that documentation for exported symbols can safely refer
  to internal pages (but internal symbols will not be included in the symbol
  index automatically). This option has no effect if @arg{single-page-p} is
  enabled.

  With @arg{include-slot-definition}, class documentation will include a list
  of direct slots.
  @see-function{liber:extract-documentation}"
  (let* ((*verbose* verbose)
         (*default-pathname-defaults* (merge-pathnames output))
         (figures-in (merge-pathnames "doc/figures/" base))
         (figures-out (if single-page-p
                          (merge-pathnames "../figures/" output)
                          (merge-pathnames "figures/" output)))
         (figures (uiop:directory-files figures-in)))

    (when *verbose*
      (format t "~&~%")
      (format t "-----------------------------------~%")
      (if single-page-p
         (format t "Generating HTML (single page)~%~%")
         (format t "Generating HTML (multiple pages)~%~%"))
      (format t "Directory (base) : ~a~%" base)
      (format t " Directory (out) : ~a~%" output)
      (format t "   Figures (in)  : ~a~&" figures-in)
      (format t "   Figures (out) : ~a~%" figures-out)
      (format t "-----------------------------------~%"))

    ;; Ensure directory for the generated documentation
    (ensure-directories-exist output)

    ;; Copy files for the documentation
    (copy-file (magic-namestring css) "index.css"
               :if-exists :rename-and-delete)
    (copy-file (magic-namestring "lambda.icon") "lambda.icon"
               :if-exists :rename-and-delete)

    ;; TODO: Only figures from the main package are copied.
    ;; Should we also collect the figures for other packages?!

    ;; Copy figures if necessary
    (unless (string= (namestring figures-in) (namestring figures-out))
      (when *verbose*
        (format t "~&")
        (format t "Copy figures~%")
        (format t "   ~a~%" figures)
        (format t "-----------------------------------~%"))
      ;; Ensure directory for the figures
      (ensure-directories-exist figures-out)
      (dolist (figure figures)
        (copy-file figure
                   (merge-pathnames figures-out
                                    (file-namestring figure))
                   :if-exists :rename-and-delete)))

    ;; Set flags for export as attributes
    (setf include-slot-definitions-p (and include-slot-definitions-p "yes"))
    (setf include-internal-symbols-p (and include-internal-symbols-p "yes"))
    (setf single-page-p (and single-page-p "yes"))
    (setf paginate-section-p (and paginate-section-p "yes"))

    ;; Initialize global for collecting statistics
    (setf *stat* (make-instance 'statistic))

    ;; Extract documentation
    (extract-documentation packages
                           output
                           :include-slot-definitions-p include-slot-definitions-p
                           :include-internal-symbols-p include-internal-symbols-p
                           :single-page-p single-page-p
                           :paginate-section-p paginate-section-p
                           :icon icon
                           :index-title index-title
                           :css "index.css"
                           :heading heading
                           :author author
                           :author-url author-url
                           :date date)

    ;; Print statistics on the console
    (when *verbose*
      (format t "-----------------------------------~%")
      (format t "Statistics about the documentation~%~%")
      (format t "   External Symbols : ~a~%" (statistic-external-symbols *stat*))
      (format t "   Internal Symbols : ~a~%" (statistic-internal-symbols *stat*))
      (format t "~%")
      (format t "   Variables    : ~a~%" (length (statistic-variables *stat*)))
      (format t "   Macros       : ~a~%" (length (statistic-macros *stat*)))
      (format t "   Operators    : ~a~%" (length (statistic-operators *stat*)))
      (format t "   Generics     : ~a~%" (length (statistic-generics *stat*)))
      (format t "   Functions    : ~a~%" (length (statistic-functions *stat*)))
      (format t "   Classes      : ~a~%" (length (statistic-classes *stat*)))
      (format t "   Types        : ~a~%" (length (statistic-types *stat*)))
      (format t "   Symbols      : ~a~%" (length (statistic-symbols *stat*)))
      (format t "~%")
      (format t "   Sum of Symbols   : ~a~%" (sum-of-symbols *stat*))
      (format t "-----------------------------------~%"))

    ;; Apply stylesheets to documentation
    (apply-stylesheet-chain ".liber.xml"
                            (list "cleanup.xsl")
                            (merge-pathnames ".liber-cleanup.xml"))
    (apply-stylesheet-chain ".liber-cleanup.xml"
                            (list (if single-page-p
                                      "html-singlepage.xsl"
                                      "html.xsl"))
                            (merge-pathnames ".liber-html.xml"))
    (apply-stylesheet-chain ".liber-html.xml"
                            (list "paginate.xsl")
                            (merge-pathnames "index.html"))
    ;; Cleanup tmp files
    (when delete-tmp-files-p
      (when (probe-file (merge-pathnames output ".liber.xml"))
        (delete-file (merge-pathnames output ".liber.xml")))
      (when (probe-file (merge-pathnames output ".liber-html.xml"))
        (delete-file (merge-pathnames output ".liber-html.xml")))
      (when (probe-file (merge-pathnames output ".liber-cleanup.xml"))
        (delete-file (merge-pathnames output ".liber-cleanup.xml"))))))

;;; ----------------------------------------------------------------------------

(xpath-sys:define-extension :liber "http://www.crategus.com/liber/")

(xpath-sys:define-xpath-function/eager :liber :escape-latex-string (x)
  ;; fixme:
  ;; \ -> $\backslash$
  ;; & -> ???
  (setf x (cl-ppcre:regex-replace-all "([#$~_^{}%])"
                                      (xpath:string-value x)
                                      "\\\\\\1"))
  (setf x (cl-ppcre:regex-replace-all "&"
                                      (xpath:string-value x)
                                      "$\\\\&$")))

(xpath-sys:define-xpath-function/eager :liber :base-uri (x)
  (xpath-protocol:base-uri (xpath:first-node x)))

(xuriella:define-extension-group :liber "http://www.crategus.com/liber/")

;;; ----------------------------------------------------------------------------

(defun replace-chars (newchar chars str)
  (substitute-if newchar #'(lambda (x) (member x (coerce chars 'list))) str))

(defun remove-chars (chars str)
  (remove-if #'(lambda (x) (member x (coerce chars 'list))) str))

(defun munge-symbol (symbol kind &optional (package nil))
  (let ((package (or package (symbol-package symbol)))
        (name (if (symbolp symbol) (symbol-name symbol) symbol)))
    (format nil "~(~A~)_~A_~(~A~)"
            (package-name package)
            kind
            (replace-chars #\_ "[ /\*%<>]" name))))

;;; ----------------------------------------------------------------------------

(defun symbol-status (symbol &optional (package nil))
  (let ((package (or package (symbol-package symbol))))
    (nth-value 1 (find-symbol (symbol-name symbol) package))))

(defun good-symbol-p (symbol other-packages)
  (and (find (symbol-package symbol) other-packages)
       (not (eq (symbol-status symbol) :internal))))

;;; ----------------------------------------------------------------------------

(defun internalp (symbol package other-packages)
  "Check whether SYMBOL is internal to some package, but not external
   to any other documented package."
  (and (iter (for package1 in (cons package other-packages))
             (thereis (eq (symbol-status symbol package1) :internal)))
       (iter (for package1 in (cons package other-packages))
             (never (eq (symbol-status symbol package1) :external)))))

;;; ----------------------------------------------------------------------------

;; <package name="gtk"
;;            id="gtk">
;;   <documentation-string>
;;     ...
;;   </documentation-string>
;;   <external-symbols>
;;     ...
;;   </external-symbols>
;;   <internal-symbols>
;;     ...
;;   </internal-symbols>
;; </package>

(defclass docstring-parser (cxml:sax-proxy)
    ((current-id :initarg :current-id
                 :initform nil
                 :accessor current-id)
     (current-symbol :initarg :current-symbol
                     :initform nil
                     :accessor current-symbol)
     (current-package :initarg :current-package
                      :initform nil
                      :accessor current-package)
     (current-name :initarg :current-name
                   :initform nil
                   :accessor current-name)
     (current-kind :accessor current-kind)
     (current-attributes :accessor current-attributes)
     (current-text :accessor current-text)))

(defun emit-package (package other-packages)
  (let ((name (string-downcase (package-name package))))
    (cxml:with-element "package"
      ;; Emit attributes
      (cxml:attribute "id" name)
      (cxml:attribute "name" (or (alias-for-symbol name) name))
      ;; Emit docstring
      (cxml:with-element "documentation-string"
        (cxml::maybe-emit-start-tag)
        (parse-docstring (make-instance 'docstring-parser
                                        :current-id name
                                        :current-symbol name
                                        :current-package package
                                        :chained-handler cxml::*sink*)
                         (or (documentation package t)
                             "No documentation string found.")))
      ;; Emit external symbols
      (cxml:with-element "external-symbols"
        (do-external-symbols (symbol package)
          (incf (statistic-external-symbols *stat*))
          (handle-symbol symbol package other-packages)))
      ;; Emit internal symbols
      (when *include-internal-symbols-p*
        (cxml:with-element "internal-symbols"
          (do-symbols (symbol package)
            (when (internalp symbol package other-packages)
              (incf (statistic-internal-symbols *stat*))
              (handle-symbol symbol package other-packages))))))))

(defun handle-symbol (symbol package other-packages)
  (when (boundp symbol)
    (emit-variable symbol package))                    ; 1. Variable
  (when (fboundp symbol)
    (cond ((macro-function symbol)
           (emit-macro symbol package))                ; 2. Macro
          ((special-operator-p symbol)
           (emit-operator symbol package))             ; 3. Special Operator
          ((typep (symbol-function symbol) 'generic-function)
           (emit-generic-function symbol package))     ; 4. Generic Function
          (t
           (emit-function symbol package))))           ; 5. Function
  (when (find-class symbol nil)
    (emit-class symbol package other-packages))        ; 6. Class
  ;; FIXME: Handle a type specifier: This is not correct for a type specifier
  ;; that has no documentation string.
  (when (and (documentation symbol 'type)
             (not (find-class symbol nil)))
    (emit-type symbol package))                        ; 7. Type
  (when (or (symbol-documentation symbol)
            (and (not (boundp symbol))
                 (not (fboundp symbol))
                 (not (find-class symbol nil))
                 (not (documentation symbol 'type))))
    (emit-symbol symbol package)))                     ; 8. Symbol

;; <symbol-definition id="gtk_sym_gtk-response-type"
;;                    name="gtk-response-type"
;;                    package="gtk"
;;                    kind="sym"
;;                    kind-name="Enum">
;;   <documentation-string>
;;     ...
;;   </documentation-string>
;; </symbol-definition>

(defun emit-symbol (symbol package)
  (let* ((id (munge-symbol symbol "sym" package))
         (name (string-downcase (symbol-name symbol)))
         (packagename (string-downcase (package-name package)))
         (sortid (format nil "~a-~a" packagename name)))
    (push symbol (statistic-symbols *stat*))
    ;; Emit symbol definition
    (cxml:with-element "symbol-definition"
      ;; Emit attributes
      (cxml:attribute "id" id)
      (cxml:attribute "sortid" sortid)
      (cxml:attribute "name" name)
      (cxml:attribute "package" packagename)
      (cxml:attribute "kind" "sym")
      (cxml:attribute "kindname" (or (alias-for-symbol symbol) "Symbol"))
      ;; Emit docstring
      (let ((docstring (symbol-documentation symbol)))
        (when docstring
          (cxml:with-element "documentation-string"
            (cxml::maybe-emit-start-tag)
            (parse-docstring (make-instance 'docstring-parser
                                            :current-id id
                                            :current-symbol symbol
                                            :current-package package
                                            :chained-handler cxml::*sink*)
                             docstring)))))))

(defun emit-variable (symbol package)
  (let* ((id (munge-symbol symbol "var" package))
         (name (string-downcase (symbol-name symbol)))
         (packagename (string-downcase (package-name package)))
         (sortid (format nil "~a-~a" packagename name)))
    (push symbol (statistic-variables *stat*))
    ;; Emit variable definition
    (cxml:with-element "variable-definition"
      ;; Emit attributes
      (cxml:attribute "id" id)
      (cxml:attribute "sortid" sortid)
      (cxml:attribute "name" name)
      (cxml:attribute "package" packagename)
      (cxml:attribute "kind" "var")
      (cxml:attribute "kindname" (or (alias-for-variable symbol) "Variable"))
      ;; Emit docstring
      (let ((docstring (documentation symbol 'variable)))
        (when docstring
          (cxml:with-element "documentation-string"
            (cxml::maybe-emit-start-tag)
            (parse-docstring (make-instance 'docstring-parser
                                            :current-id id
                                            :current-symbol symbol
                                            :current-package package
                                            :chained-handler cxml::*sink*)
                             docstring)))))))

(defun emit-function (symbol package)
  (let* ((id (munge-symbol symbol "fun" package))
         (name (string-downcase (symbol-name symbol)))
         (packagename (string-downcase (package-name package)))
         (sortid (format nil "~a-~a" packagename name)))
    (push symbol (statistic-functions *stat*))
    ;; Emit function definition
    (cxml:with-element "function-definition"
      ;; Emit attributes
      (cxml:attribute "id" id)
      (cxml:attribute "sortid" sortid)
      (cxml:attribute "name" name)
      (cxml:attribute "package" packagename)
      (cxml:attribute "kind" "fun")
      (cxml:attribute "kindname" (or (alias-for-function symbol) "Function"))
      ;; Emit lambda list
      (cxml:with-element "lambda-list"
        (dolist (arg (lambda-list (symbol-function symbol)))
          (cxml:with-element "elt"
            (cxml:text (write-to-string arg
                                        :pretty t
                                        :escape nil
                                        :case :downcase)))))
      ;; Emit docstring
      (let ((docstring (documentation symbol 'function)))
        (when docstring
          (cxml:with-element "documentation-string"
            (cxml::maybe-emit-start-tag)
            (parse-docstring (make-instance 'docstring-parser
                                            :current-id id
                                            :current-symbol symbol
                                            :current-package package
                                            :chained-handler cxml::*sink*)
                             docstring)))))))

(defun emit-generic-function (symbol package)
  (let* ((id (munge-symbol symbol "fun" package))
         (name (string-downcase (symbol-name symbol)))
         (packagename (string-downcase (package-name package)))
         (sortid (format nil "~a-~a" packagename name)))
    (push symbol (statistic-generics *stat*))
    ;; Emit generic function definition
    (cxml:with-element "generic-definition"
      ;; Emit attributes
      (cxml:attribute "id" id)
      (cxml:attribute "sortid" sortid)
      (cxml:attribute "name" name)
      (cxml:attribute "package" packagename)
      (cxml:attribute "kind" "fun")
      (cxml:attribute "kindname" (or (alias-for-function symbol)
                                     "Generic Function"))
      ;; Emit lambda list
      (cxml:with-element "lambda-list"
        (dolist (arg (lambda-list (symbol-function symbol)))
          (cxml:with-element "elt"
            (cxml:text (write-to-string arg
                                        :pretty t
                                        :escape nil
                                        :case :downcase)))))
      ;; Emit docstring
      (let ((docstring (documentation symbol 'function)))
        (when docstring
          (cxml:with-element "documentation-string"
            (cxml::maybe-emit-start-tag)
            (parse-docstring (make-instance 'docstring-parser
                                            :current-id id
                                            :current-symbol symbol
                                            :current-package package
                                            :chained-handler cxml::*sink*)
                             docstring)))))))

(defun emit-operator (symbol package)
  (let* ((id (munge-symbol symbol "fun" package))
         (name (string-downcase (symbol-name symbol)))
         (packagename (string-downcase (package-name package)))
         (sortid (format nil "~a-~a" packagename name)))
    (push symbol (statistic-operators *stat*))
    ;; Emit special operator definition
    (cxml:with-element "operator-definition"
      ;; Emit attributes
      (cxml:attribute "id" id)
      (cxml:attribute "sortid" sortid)
      (cxml:attribute "name" name)
      (cxml:attribute "package" packagename)
      (cxml:attribute "kind" "fun")
      (cxml:attribute "kindname"
                      (or (alias-for-function symbol) "Special Operator"))
      ;; Emit lambda list
      (cxml:with-element "lambda-list"
        (dolist (arg (lambda-list (symbol-function symbol)))
          (cxml:with-element "elt"
            (cxml:text (write-to-string arg
                                        :pretty t
                                        :escape nil
                                        :case :downcase)))))
      ;; Emit docstring
      (let ((docstring (documentation symbol 'function)))
        (when docstring
          (cxml:with-element "documentation-string"
            (cxml::maybe-emit-start-tag)
            (parse-docstring (make-instance 'docstring-parser
                                            :current-id id
                                            :current-symbol symbol
                                            :current-package package
                                            :chained-handler cxml::*sink*)
                             docstring)))))))

(defun emit-macro (symbol package)
  (let* ((id (munge-symbol symbol "fun" package))
         (name (string-downcase (symbol-name symbol)))
         (packagename (string-downcase (package-name package)))
         (sortid (format nil "~a-~a" packagename name)))
    (push symbol (statistic-macros *stat*))
    ;; Emit macro definition
    (cxml:with-element "macro-definition"
      ;; Emit attributes
      (cxml:attribute "id" id)
      (cxml:attribute "sortid" sortid)
      (cxml:attribute "name" name)
      (cxml:attribute "package" packagename)
      (cxml:attribute "kind" "fun")
      (cxml:attribute "kindname" (or (alias-for-function symbol) "Macro"))
      ;; Emit lambda list
      (cxml:with-element "lambda-list"
        (dolist (arg (lambda-list (macro-function symbol)))
          (cxml:with-element "elt"
            (cxml:text (write-to-string arg
                                        :pretty t
                                        :escape nil
                                        :case :downcase)))))
      ;; Emit docstring
      (let ((docstring (documentation symbol 'function)))
        (when docstring
          (cxml:with-element "documentation-string"
            (cxml::maybe-emit-start-tag)
            (parse-docstring (make-instance 'docstring-parser
                                            :current-id id
                                            :current-symbol symbol
                                            :current-package package
                                            :chained-handler cxml::*sink*)
                             docstring)))))))

(defun emit-type (symbol package)
  (let* ((id (munge-symbol symbol "type" package))
         (name (string-downcase (symbol-name symbol)))
         (packagename (string-downcase (package-name package)))
         (sortid (format nil "~a-~a" packagename name)))
    (push symbol (statistic-types *stat*))
    ;; Emit type definition
    (cxml:with-element "type-definition"
      ;; Emit attributes
      (cxml:attribute "id" id)
      (cxml:attribute "sortid" sortid)
      (cxml:attribute "name" name)
      (cxml:attribute "package" packagename)
      (cxml:attribute "kind" "type")
      (cxml:attribute "kindname" (or (alias-for-type symbol) "Type"))
      ;; Emit docstring
      (let ((docstring (documentation symbol 'type)))
        (when docstring
          (cxml:with-element "documentation-string"
            (cxml::maybe-emit-start-tag)
            (parse-docstring (make-instance 'docstring-parser
                                            :current-id id
                                            :current-symbol symbol
                                            :current-package package
                                            :chained-handler cxml::*sink*)
                             docstring)))))))

;;    <slot id="gtk_slot_label"
;;          name="label"
;;          package="gtk"
;;          allocation="keyword_symbol_gobject-property"
;;          type="T">
;;      <initargs>
;;        <initarg id="keyword_symbol_label" name="label" package="keyword"/>
;;      </initargs>
;;      <readers>
;;        <reader id="gtk_symbol_gtk-frame-label" name="gtk-frame-label" package="gtk"/>
;;      </readers>
;;      <documentation-string>
;;        The <code>"label"</code> property of type  <code>:string</code> (Read / Write) <br></br>
;;        Text of the frame's label. <br></br>  Default value: <code>nil</code></documentation-string>
;;    </slot>

(defun emit-slot (class package slot-def)
  (let* ((name (closer-mop:slot-definition-name slot-def))
         (symbol (make-symbol
                   (string-upcase
                     (format nil "~A-~A" (class-name class) name))))
         (id (munge-symbol symbol "slot" package))
         (classid (munge-symbol (class-name class) "class" package))
         (packagename (string-downcase (package-name package)))
         (sortid (format nil "~a-~a" packagename symbol)))
    (cxml:with-element "slot"
      (cxml:attribute "id" id)
      (cxml:attribute "sortid" sortid)
      (cxml:attribute "classid" classid)
      (cxml:attribute "name" (string-downcase (symbol-name name)))
      (cxml:attribute "package" packagename)

      ;; At this time not used in the style sheets
      (cxml:attribute "allocation"
        (munge-symbol (closer-mop:slot-definition-allocation slot-def) "symbol"))

      ;; At this time not used in the style sheets
      (cxml:attribute "type"
        ;; may be a complicated typespec
        (format nil "~A" (closer-mop:slot-definition-type slot-def)))
      ;; At this time not used in the style sheets
      (cxml:with-element "initargs"
        (dolist (ia (closer-mop:slot-definition-initargs slot-def))
          (cxml:with-element "initarg"
            (cxml:attribute "id" (munge-symbol ia "symbol"))
            (cxml:attribute "name" (string-downcase (symbol-name ia)))
            (cxml:attribute "package"
                            (string-downcase (package-name (symbol-package ia)))))))
      ;; At this time not used in the style sheets
      (cxml:with-element "readers"
        (dolist (reader (closer-mop:slot-definition-readers slot-def))
          (cxml:with-element "reader"
            (cxml:attribute "id" (munge-symbol reader "symbol"))
            (cxml:attribute "name" (string-downcase (symbol-name reader)))
            (cxml:attribute "package" (string-downcase (package-name package))))))
      ;; At this time not used in the style sheets
      (cxml:with-element "writers"
        (dolist (writer (closer-mop:slot-definition-writers slot-def))
          (let ((symbol (make-symbol
                          (string-upcase
                            (remove-chars "()" (format nil "~a" writer))))))
            (cxml:with-element "writer"
            (cxml:attribute "id" (munge-symbol symbol "symbol" package))
            (cxml:attribute "name" (string-downcase (symbol-name symbol)))
            (cxml:attribute "package" (string-downcase (package-name package)))))))
      ;; Emit docstring
      (let ((docstring (documentation slot-def t)))
        (when docstring
          (cxml:with-element "documentation-string"
            (cxml::maybe-emit-start-tag)
            (parse-docstring (make-instance 'docstring-parser
                                            :current-id id
                                            :current-symbol class
                                            :current-package package
                                            :chained-handler cxml::*sink*)
                             docstring)))))))

;;;<class-definition id="test_class_my-class"
;;;                  name="my-class"
;;;                  package="test"
;;;                  kind="class"
;;;                  kind-name="Class">
;;;  <cpl>
;;;    <superclass status="EXTERNAL" name="t" package="common-lisp"/>
;;;  </cpl>
;;;  <subclasses>
;;;    <subclass status="EXTERNAL"
;;;              id="test_class_derived-class"
;;;              name="derived-class"
;;;              package="test"/>
;;;  </subclasses>
;;;  <direct-slots>
;;;    <slot id="test_slot_my-class-first-slot"
;;;          classid="test_class_my-class"
;;;          name="first-slot"
;;;          package="test"
;;;          allocation="keyword_symbol_instance"
;;;          type="T">
;;;      <initargs>
;;;        <initarg id="keyword_symbol_first-slot" name="first-slot" package="keyword"/>
;;;      </initargs>
;;;      <readers/>
;;;      <documentation-string>
;;;        This is the documentation for the first slot.
;;;      </documentation-string>
;;;    </slot>
;;;    <slot id="test_slot_my-class-second-slot"
;;;          classid="test_class_my-class"
;;;          name="second-slot"
;;;          package="test"
;;;          allocation="keyword_symbol_instance"
;;;          type="T">
;;;      <initargs>
;;;        <initarg id="keyword_symbol_second-slot" name="second-slot" package="keyword"/>
;;;      </initargs>
;;;      <readers/>
;;;      <documentation-string>
;;;        This is the documentation for the second slot.
;;;      </documentation-string>
;;;    </slot>
;;;  </direct-slots>
;;;  <documentation-string>
;;;    This is the documentation for the class myClass
;;;  </documentation-string>
;;;</class-definition>

(defun clean-class-list (classes)
  (let ((l classes))
    (dolist (class classes)
      (let ((status (symbol-status (class-name class))))
        (when (or (not status) (eq status :internal))
          (setq l (remove class l)))))
    l))

(defun emit-class (symbol package other-packages)
  (let* ((class (find-class symbol))
         (id (munge-symbol symbol "class" package))
         (name (string-downcase (symbol-name symbol)))
         (packagename (string-downcase (package-name package)))
         (sortid (format nil "~a-~a" packagename name))
         ;; FIXME: Does this work well for a class of type condition
         ;; and for a built in class !?
         (ctype (string-downcase (symbol-name (type-of class)))))
    (push symbol (statistic-classes *stat*))
    ;; Emit class definition
    (cxml:with-element (format nil "~a-definition" ctype)
      ;; Emit attributes
      (cxml:attribute "id" id)
      (cxml:attribute "sortid" sortid)
      (cxml:attribute "name" name)
      (cxml:attribute "package" packagename)
      (cxml:attribute "kind" "class")
      (cxml:attribute "kindname"
                      (or (alias-for-class symbol)
                          (cdr (assoc ctype
                                      '(("structure-class" . "Struct")
                                        ("condition-class" . "Condition"))
                                      :test #'string=))
                          "Class"))
      (closer-mop:finalize-inheritance class)
      ;; Emit superclasses
      (cxml:with-element "cpl"
        (dolist (super (cdr (clean-class-list
                              (closer-mop:class-precedence-list class))))
          (let* ((symbol (class-name super))
                 (package (symbol-package symbol)))
            (cxml:with-element "superclass"
              (cxml:attribute "status" (symbol-name (symbol-status symbol)))
              (when (good-symbol-p symbol other-packages)
                (cxml:attribute "id" (munge-symbol symbol "class" package)))
              (cxml:attribute "name" (string-downcase (symbol-name symbol)))
              (cxml:attribute "package"
                              (string-downcase (package-name package)))))))
      ;; Emit subclasses
      (cxml:with-element "subclasses"
        (labels ((recurse (c)
                   (dolist (sub (closer-mop:class-direct-subclasses c))
                     (let* ((symbol (class-name sub))
                            (package (symbol-package symbol)))
                     (when (good-symbol-p symbol other-packages)
                       (cxml:with-element "subclass"
                       (cxml:attribute "status"
                                       (symbol-name (symbol-status symbol)))
                       (cxml:attribute "id" (munge-symbol symbol "class" package))
                       (cxml:attribute "name"
                                       (string-downcase (symbol-name symbol)))
                       (cxml:attribute "package"
                                       (string-downcase (package-name package)))
                       (recurse sub)))))))
          (recurse class)))
      ;; Emit slots
      (when (and *include-slot-definitions-p*
                 (not (typep class 'structure-class)))
        (cxml:with-element "direct-slots"
          (dolist (slot (closer-mop:class-direct-slots class))
            (emit-slot class package slot))))
      ;; Emit docstring
      (let ((docstring (documentation class t)))
        (when docstring
          (cxml:with-element "documentation-string"
            (cxml::maybe-emit-start-tag)
            (parse-docstring (make-instance 'docstring-parser
                                            :current-id id
                                            :current-symbol symbol
                                            :current-package package
                                            :chained-handler cxml::*sink*)
                             docstring)))))))

;;; ----------------------------------------------------------------------------

(defun emit-error (handler message)
  (if (symbolp (current-symbol handler))
      (error "In symbol ~a:~a: ~a"
             (package-name (current-package handler))
             (symbol-name (current-symbol handler))
             message)
      (error "In package ~a: ~a"
             (current-symbol handler)
             message)))

(defun characters (handler str)
  (let ((lines (coerce (split-sequence:split-sequence #\newline str) 'vector))
        (skip nil))
    ;; Handle first line
    (sax:characters handler (elt lines 0))
    ;; Handle more lines
    (when (> (length lines) 1)
      (iter (for i from 1 below (1- (length lines)))
            (for line = (elt lines i))
            (cond ((zerop (length (string-trim " " line)))
                   (unless skip
                     (sax:start-element handler nil "break" "break" nil)
                     (sax:end-element handler nil "break" "break"))
                   (setf skip t))
                  (t
                   (sax:characters handler (string #\newline))
                   (sax:characters handler line)
                   (setf skip nil))))
      ;; Handle last line
      (sax:characters handler (elt lines (1- (length lines)))))))

(defun read-delimited-string (handler stream bag &optional eat-limit)
  (let ((out (make-string-output-stream)))
    (iter (for c = (read-char stream nil))
          (when (null c)
            (emit-error handler "Unexpected end of documentation string."))
          ;; FIXME: Improve this implementation
          (when (eql c #\@)
            (cond ((eql (peek-char nil stream nil) #\])
                   (write-char (read-char stream nil) out)
                   (setq c (read-char stream nil)))))
          (when (find c bag)
            (unless eat-limit
              (unread-char c stream))
            (return (get-output-stream-string out)))
          (write-char c out))))

(defun parse-docstring-element-section (handler stream name arg close)
  (let* ((attrs '())
         (package (current-package handler))
         (id (munge-symbol (string-downcase arg) "section" package)))
    (push (sax:make-attribute :qname "package" :value (package-name package))
          attrs)
    (push (sax:make-attribute :qname "id" :value id) attrs)
    (when arg
      (push (sax:make-attribute :qname name :value arg) attrs))
    (sax:start-element handler nil name name attrs)
    (parse-docstring-1 handler stream close)
    (sax:end-element handler nil name name)))

(defun parse-docstring-element-signal (handler stream name arg close)
  (let* ((attrs '())
         (package (current-package handler))
         (arg1 (split-sequence:split-sequence #\: arg))
         ;; e.g. action-group::action-removed -> action-group-action-removed
         (arg2 (format nil "~a-~a" (first arg1) (car (last arg1))))
         ;; e.g. gio_signal_action-group-action-removed
         (id (munge-symbol (string-downcase arg2) "signal" package)))
    (push (sax:make-attribute :qname "package" :value (package-name package))
          attrs)
    (push (sax:make-attribute :qname "id" :value id) attrs)
    (when arg
      (push (sax:make-attribute :qname name :value (car (last arg1))) attrs))
    (sax:start-element handler nil name name attrs)
    (parse-docstring-1 handler stream close)
    (sax:end-element handler nil name name)))

(defun parse-docstring-element-property (handler stream name arg close)
  (let* ((attrs '())
         (package (current-package handler))
         (arg1 (split-sequence:split-sequence #\: arg))
         ;; e.g. action-group::action-removed -> action-group-action-removed
         (arg2 (format nil "~a-~a" (first arg1) (car (last arg1))))
         ;; e.g. gio_prop_action-group-action-removed
         (id (munge-symbol (string-downcase arg2) "prop" package)))
    (push (sax:make-attribute :qname "package" :value (package-name package))
          attrs)
    (push (sax:make-attribute :qname "id" :value id) attrs)
    (when arg
      (push (sax:make-attribute :qname name :value (car (last arg1))) attrs))
    (sax:start-element handler nil name name attrs)
    (parse-docstring-1 handler stream close)
    (sax:end-element handler nil name name)))

(defun parse-docstring-element (handler stream name)
  (let ((close t)
        (arg nil)
        (attrs '())
        (first-char (read-char stream)))
    ;; Look for an argument, e.g. code in @begin[code]{table}
    (when (eql first-char #\[)
      (setf arg (read-delimited-string handler stream "]" t))
      (setf first-char (read-char stream)))
    ;; We expect an opening brace
    (unless (eq first-char #\{)
      (emit-error handler "Expected opening brace."))
    ;; We found the start tag begin and store the close tag
    (when (string= name "begin")
      (setf name (read-delimited-string handler stream "}" t))
      (setf close name))
    (cond (;; Handle @begin[...]{section} or @begin[...]{subsection}
           (or (equal name "section")
               (equal name "subsection"))
           (parse-docstring-element-section handler stream name arg close))
          ;; Handle @begin[...]{signal}
          ((string= name "signal")
           (parse-docstring-element-signal handler stream name arg close))
          ;; Handle @begin[...]{property}
          ((string= name "property")
           (parse-docstring-element-property handler stream name arg close))
          (t
           (when arg
             (push (sax:make-attribute :qname name :value arg) attrs))
           (sax:start-element handler nil name name attrs)
           (parse-docstring-1 handler stream close)
           (sax:end-element handler nil name name)))))

(defun parse-docstring-1 (handler stream close)
  (let ((out (make-string-output-stream)))
    (iter (for c = (read-char stream nil))
          (cond ((null c)
                 (when close
                   (emit-error handler "Unexpected end of documentation string."))
                 (return))
                ((eql c #\@)
                 (cond
                   (;; handle "@\"
                    (eql (peek-char nil stream nil) #\})
                    (write-char (read-char stream) out))
                   (;; handle "@@"
                    (eql (peek-char nil stream nil) #\@)
                    (write-char (read-char stream) out))
                   (;; handle "@]"
                    (eql (peek-char nil stream nil) #\])
                    (write-char (read-char stream) out))
                   (t
                    ;; At this point we have found some command like @begin.
                    ;; First we handle all characters read to this point.
                    (characters handler (get-output-stream-string out))
                    ;; Now we read the command.
                    (let ((name (read-delimited-string handler stream "[{ :")))
                      ;; Do we have found @end{} ?
                      (when (string= name "end")
                        ;; Yes. Check if we have a corresponding opening tag.
                        (read-char stream)
                        (unless (equal (read-delimited-string handler
                                                              stream "}" t)
                                       close)
                          (emit-error handler
                            (format nil "Invalid close tag found. Expected: ~a"
                                        close)))
                        (return))
                      ;; Handle the command.
                      (parse-docstring-element handler stream name)))))
                ((eql c #\})
                 (when (eq close t)
                   (return))
                 (emit-error handler "Unexpected closing brace."))
                (t
                 (write-char c out))))

    ;; Handle all remaining characters we have written out
    (characters handler (get-output-stream-string out))))

(defun parse-docstring (handler docstring)
  (when docstring
    (with-input-from-string (str docstring)
      (parse-docstring-1 handler str nil))))

(defparameter *qnames*
              '(("ref"               . "section")

                ("val"               . "sym")
                ("sym"               . "sym")
                ("symbol"            . "sym")
                ("see-symbol"        . "sym")
                ("about-symbol"      . "sym")

                ("var"               . "var")
                ("see-variable"      . "var")
                ("about-variable"    . "var")

                ("fun"               . "fun")
                ("setf"              . "fun")
                ("see-function"      . "fun")
                ("about-function"    . "fun")

                ("gen"               . "fun")
                ("see-generic"       . "fun")
                ("about-generic"     . "fun")

                ("operator"          . "fun")
                ("see-operator"      . "fun")
                ("about-operator"    . "fun")

                ("macro"             . "fun")
                ("see-macro"         . "fun")
                ("about-macro"       . "fun")

                ("see-constructor"   . "fun")
                ("about-constructor" . "fun")

                ("type"              . "type")
                ("see-type"          . "type")
                ("about-type"        . "type")

                ("class"             . "class")
                ("see-class"         . "class")
                ("about-class"       . "class")

                ("struct"            . "class")
                ("see-struct"        . "class")
                ("about-struct"      . "class")

                ("condition"         . "class")
                ("see-condition"     . "class")
                ("about-condition"   . "class")

                ("slot"              . "slot")
                ("see-slot"          . "fun")
                ("about-slot"        . "fun")

                ("sig"               . "sig")
                ("prop"              . "prop")
                ))

(defmethod sax:start-element ((handler docstring-parser) uri lname qname attrs)
  (declare (ignore uri lname))
  (let ((element (assoc qname *qnames* :test #'string=)))
    (if element
        (setf (current-name handler) qname
              (current-kind handler) (cdr element)
              (current-attributes handler) attrs
              (current-text handler) "")
        (call-next-method))))

(defmethod sax:characters ((handler docstring-parser) data)
  (if (current-name handler)
      (setf (current-text handler)
            (concatenate 'string (current-text handler) data))
      (call-next-method)))

;; TODO: Improve the implementation
(defun make-id-for-slot (name class text)
  (declare (ignore name))
  (let ((name (read-from-string text)))
    (destructuring-bind (text1 &optional text2)
        (split-sequence:split-sequence #\: class)
      (when text2
        (let ((package (find-package (string-upcase text1))))
          (when package
            (multiple-value-bind (symbol status)
                (find-symbol (string-upcase text2) package)
              (declare (ignore symbol))
              (when (eq :external status)
                (munge-symbol (format nil "~a-~a" text2 name)
                              "slot"
                              (symbol-package
                                (find-symbol (string-upcase text2) package)))))))))))

(defun make-classid-for-slot (class)
  (destructuring-bind (text1 &optional text2)
      (split-sequence:split-sequence #\: class)
    (when text2
      (let ((package (find-package (string-upcase text1))))
        (when package
          (multiple-value-bind (symbol status)
              (find-symbol (string-upcase text2) package)
            (declare (ignore symbol))
            (when (eq :external status)
              (munge-symbol text2
                            "class"
                            (symbol-package
                              (find-symbol (string-upcase text2) package))))))))))

(defun make-id-for-signal (name value text)
  (declare (ignore name))
  (let* ((text (first (split-sequence:split-sequence #\: text)))
         (id (format nil "~a-~a"
                         (munge-symbol (read-from-string value) "signal")
                          text)))
    id))

(defun make-id-for-prop (name value text)
  (declare (ignore name))
  (let* ((text (first (split-sequence:split-sequence #\: text)))
         (id (format nil "~a-~a"
                         (munge-symbol (read-from-string value) "prop")
                          text)))
    id))

(defmethod sax:end-element ((handler docstring-parser) uri lname qname)
  (declare (ignore uri lname))
  (when (equal qname (current-name handler))
    (let* ((name (current-name handler))
           (next (cxml:proxy-chained-handler handler))
           (attrs (current-attributes handler))
           (attr (first attrs))
           (text (current-text handler))
           (kind (current-kind handler))
           (value nil)
           (id (cond ;; Handle @slot[Value]{TEXT}
                     ((string= kind "slot")
                      (setf value (sax::standard-attribute-value attr))
                      (make-id-for-slot name value text))
                     ;; Handle @sig[VALUE]{TEXT}
                     ((string= kind "sig")
                      (setf value (sax::standard-attribute-value attr))
                      (make-id-for-signal name value text))
                     ;; Handle @prop[VALUE]{TEXT}
                     ((string= kind "prop")
                      (setf value (sax::standard-attribute-value attr))
                      (make-id-for-prop name value text))
                     ;; Handle something like @sym[VALUE]{TEXT}
                     ((and attr
                           (setf value
                                 (sax::standard-attribute-value attr)))
                      (destructuring-bind (value1 &optional value2)
                          (split-sequence:split-sequence #\: value)
                        (if value2
                            (let ((package (find-package (string-upcase value1))))
                              ;; TODO: The implementation can be optimized.
                              (if package
                                  (multiple-value-bind (symbol status)
                                      (find-symbol (string-upcase value2) package)
                                    (declare (ignore symbol))
                                    (if (eq :external status)
                                        (munge-symbol (read-from-string value)
                                                      kind)
                                        nil))
                                  nil))
                            (munge-symbol (read-from-string value) kind))))
                     (t
                      ;; Handle something like @fun{TEXT}
                      (destructuring-bind (text1 &optional text2)
                          (split-sequence:split-sequence #\: text)
                        (if text2
                            (let ((package (find-package (string-upcase text1))))
                              ;; TODO: The implementation can be optimized.
                              (if package
                                  (multiple-value-bind (symbol status)
                                      (find-symbol (string-upcase text2) package)
                                    (declare (ignore symbol))
                                    (if (eq :external status)
                                        (munge-symbol (read-from-string text)
                                                      (current-kind handler))
                                        nil))
                                  nil))
                            (munge-symbol (read-from-string text)
                                          (current-kind handler)))))))
           (classid (cond ((string= (current-kind handler) "slot")
                           (let ((class (sax::standard-attribute-value attr)))
                             (make-classid-for-slot class)))
                           ;; Improve  the implementation for "prop" and "sig"
                           ((string= (current-kind handler) "prop")
                            (let ((value (sax::standard-attribute-value attr)))
                              (munge-symbol (read-from-string value) "class")))
                           ((string= (current-kind handler) "sig")
                            (let ((value (sax::standard-attribute-value attr)))
                              (munge-symbol (read-from-string value) "class"))))))
      (when classid
        (push (sax:make-attribute :qname "classid" :value classid) attrs))
      (when id
        (push (sax:make-attribute :qname "id" :value id) attrs))
      (sax:start-element next nil name name attrs)
      (sax:characters next text)
      (setf (current-name handler) nil)))
  (call-next-method))

;;; --- End of file liber.lisp -------------------------------------------------
