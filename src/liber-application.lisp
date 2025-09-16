(in-package :liber)

(defvar *app*)

(gobject:define-gobject-subclass "Liber" liber
  (:superclass gtk:application
   :export t
   :interfaces ())
  ((asdf
    liber-asdf
    "asdf" "gchararray" t t)
   (asdf-entry
    liber-asdf-entry
    "asdf-entry" "GtkWidget" t t)
   (main-package
    liber-main-package
    "main-package" "gchararray" t t)
   (main-package-entry
    liber-main-package-entry
    "main-package-entry" "GtkWidget" t t)
   (other-packages
    liber-other-packages
    "other-packages" "gchararray" t t)
   (other-packages-entry
    liber-other-packages-entry
    "other-packages-entry" "GtkWidget" t t)
   (base-directory
    liber-base-directory
    "base-directory" "gchararray" t t)
   (base-directory-entry
    liber-base-directory-entry
    "base-directory-entry" "GtkWidget" t t)
   (output-directory
    liber-output-directory
    "output-directory" "gchararray" t t)
   (output-directory-entry
    liber-output-directory-entry
    "output-directory-entry" "GtkWidget" t t)
   (css-file
    liber-css-file
    "css-file" "gchararray" t t)
   (css-file-entry
    liber-css-file-entry
    "css-file-entry" "GtkWidget" t t)
   (icon-file
    liber-icon-file
    "icon-file" "gchararray" t t)
   (icon-file-entry
    liber-icon-file-entry
    "icon-file-entry" "GtkWidget" t t)
  ))

;;; ----------------------------------------------------------------------------

(defun set-default-values (application)
  (setf (liber-asdf application) "cl-cffi-gtk4"
        (liber-main-package application) "GTK"
        (liber-other-packages application) "GDK, GSK, GDK-PIXBUF, GLIB, GOBJECT, GIO, PANGO, CAIRO, GRAPHENE"
        (liber-base-directory application) "default"
        (liber-output-directory application) "doc/"
        (liber-css-file application) "default.css"
        (liber-icon-file application) "lambda.icon"))

(defun set-application-entries (builder application)

  (setf (liber-asdf-entry application) (gtk:builder-object builder "asdf_entry"))
  (setf (gtk:entry-buffer-text (gtk:entry-buffer (liber-asdf-entry application)))
        (liber-asdf application))

  (setf (liber-main-package-entry application) (gtk:builder-object builder "main_package_entry"))
  (setf (gtk:entry-buffer-text (gtk:entry-buffer (liber-main-package-entry application)))
        (liber-main-package application))

  (setf (liber-other-packages-entry application) (gtk:builder-object builder "other_packages_entry"))
  (setf (gtk:entry-buffer-text (gtk:entry-buffer (liber-other-packages-entry application)))
        (liber-other-packages application))

  (setf (liber-base-directory-entry application) (gtk:builder-object builder "base_directory_entry"))
  (setf (gtk:entry-buffer-text (gtk:entry-buffer (liber-base-directory-entry application)))
        (liber-base-directory application))

  (setf (liber-output-directory-entry application) (gtk:builder-object builder "output_directory_entry"))
  (setf (gtk:entry-buffer-text (gtk:entry-buffer (liber-output-directory-entry application)))
        (liber-output-directory application))

  (setf (liber-css-file-entry application) (gtk:builder-object builder "css_file_entry"))
  (setf (gtk:entry-buffer-text (gtk:entry-buffer (liber-css-file-entry application)))
        (liber-css-file application))

  (setf (liber-icon-file-entry application) (gtk:builder-object builder "icon_file_entry"))
  (setf (gtk:entry-buffer-text (gtk:entry-buffer (liber-icon-file-entry application)))
        (liber-icon-file application))


  ;; FIXME: Does this binding work?
  (g:object-bind-property application
                          "asdf"
                          (gtk:entry-buffer (liber-asdf-entry application))
                          "text"
                          :bidirectional)
)

;;; ----------------------------------------------------------------------------

(defun asdf-entry-activate (entry)
  (let* ((buffer (gtk:entry-buffer entry))
         (text (string-downcase (gtk:entry-buffer-text buffer)))
         (asdf (asdf:find-system text nil))
         (attrlist (pango:attr-list-new))
         (attr1 (pango:attribute-new :foreground 0 #xffffffff 0 0 0))
         (attr2 (pango:attribute-new :foreground 0 #xffffffff #xffff 0 0)))

    (if asdf
        (pango:attr-list-insert attrlist attr1)
        (pango:attr-list-insert attrlist attr2))

    (setf (gtk:entry-attributes entry) attrlist)
    (setf (gtk:entry-buffer-text buffer) text)
    (setf (gtk:editable-position entry) -1)

    (when asdf
      (let ((base (namestring (asdf:component-pathname asdf))))
        (setf (liber-base-directory *app*) base)
        (setf (gtk:entry-buffer-text (gtk:entry-buffer (liber-base-directory-entry *app*))) base)))

))

(defun main-package-entry-activate (entry)
  (let* ((buffer (gtk:entry-buffer entry))
         (text (string-upcase (gtk:entry-buffer-text buffer)))
         (package (find-package text))
         (attrlist (pango:attr-list-new))
         (attr1 (pango:attribute-new :foreground 0 #xffffffff 0 0 0))
         (attr2 (pango:attribute-new :foreground 0 #xffffffff #xffff 0 0)))

    (if package
        (pango:attr-list-insert attrlist attr1)
        (pango:attr-list-insert attrlist attr2))

    (setf (gtk:entry-attributes entry) attrlist)
    (setf (gtk:entry-buffer-text buffer) text)
    (setf (gtk:editable-position entry) -1)

))

(defun other-packages-entry-activate (entry)
  (let* ((buffer (gtk:entry-buffer entry))
         (text (string-upcase (gtk:entry-buffer-text buffer)))
         ;; TODO: Use split-sequence-if
         (packages (split-sequence:split-sequence #\, text)))

    (dolist (package packages)
      (format t "   package : ~a~%" package)
    )



))

;;; ----------------------------------------------------------------------------

(defun liber (&rest argv)
  (let (;; Create application
        (app (make-instance 'liber
                            :flags :default-flags
                            :application-id "com.crategus.liber")))
    (setf *app* app)
    ;; Connect signal "activate" to the application
    (g:signal-connect app "activate"
        (lambda (application)
          ;; Create application window
          (let* ((path (glib-sys:sys-path "src/liber.ui"))
                 (builder (gtk:builder-new-from-file path))
                 (window (gtk:builder-object builder "liberwindow")))
            (setf (gtk:window-application window) application)

            (set-default-values application)
            (set-application-entries builder application)

            (g:signal-connect (liber-asdf-entry application)
                              "activate"
                              #'asdf-entry-activate)
            (g:signal-connect (liber-main-package-entry application)
                              "activate"
                              #'main-package-entry-activate)
            (g:signal-connect (liber-other-packages-entry application)
                              "activate"
                              #'other-packages-entry-activate)

            ;; Present the application window
            (gtk:window-present window))))
  ;; Run application
  (g:application-run app argv)))
