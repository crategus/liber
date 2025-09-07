;;; ----------------------------------------------------------------------------
;;; package.lisp
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

(defpackage :liber
  (:use :iterate :common-lisp)
  (:export #:alias-for-symbol
           #:alias-for-variable
           #:alias-for-function
           #:alias-for-type
           #:alias-for-class

           #:slot-documentation
           #:symbol-documentation

           #:generate-html-documentation
           #:extract-documentation

           #:liber))

(setf (documentation (find-package :liber) t)
 "@version{2025-09-06}
  The Liber library is a  HTML documentation generation tool for Common Lisp
  packages. It extracts documentation strings written in a custom markup
  language and generates either multiple HTML pages or a single HTML page for
  the documentation.

  The Liber library is a forck of the @url[http://www.lichteblau.com/atdoc/]{atdoc}
  library written by David Lichteblau. It is developped using SBCL on Ubuntu.
  The library runs sucessfully on Windows. The Liber library is licensed under
  the MIT license.
  @begin[Overview]{section}
    @subheading{Author}
    Copyright (C) 2006 - 2008 David Lichteblau @br{}
    Copyright (C) 2012 . 2025 Dieter Kaiser

    @subheading{Homepage}
    The documentation is available at
    @url[http://www.crategus.com/books/liber/index.html]{Liber API documentation}.

    @subheading{Download}
    Get the source code of the Liber library at
    @url[github]{https://github.com/crategus/liber}.

    @subheading{Installation}
    The Liber library is ASDF-installable:
    @begin{pre}
(asdf:load-system :liber)
    @end{pre}
    @subheading{Dependencies}
    The Liber library needs Closure XML, Split sequence, Xuriella XSLT,
    Closer-MOP, and their dependencies.
  @end{section}
  @begin[Output formats]{section}
    The Liber library can currently generate documentation in these formats:
    @begin{itemize}
      @item{HTML, one page for each definition, with extra pages containing the
        package and overview text for its definitions, see the example
        @url[http://www.crategus.com/books/liber/example/index.html]{blocks-world API documentation}.}
      @item{HTML, all on one page, see the example
        @url[http://www.crategus.com/books/liber/example/single-page/index.html]{blocks-world API documentation (single-page)}.}
    @end{itemize}
  @end{section}
  @begin[Sample Documentation]{section}
    As an example, code from the book Lisp (3rd edition) by Winston and Horn is
    chosen. You can find the code with an ASDF definition in the @file{example/}
    subdirectory of the Liber sources so that you can easily compile it
    yourself. The code included is the Blocks World, from chapter 21: \"The
    Blocks World with Classes and Methods\" and chapter 22: \"Answering
    Questions about Goals\". Note that the source code from the book has been
    taken from the publically available lisp3 tarball and is covered by its own
    license, different from the license of the Liber library.

    In general, you will write a Lisp script that loads the necessary packages
    for the documentation, configures the environment and executes the HTML
    generation. Below is an example from the @file{generate-html.lisp} file for
    the @code{BLOCKS-WORD} example that comes with the library.
    @begin{pre}
;; Make HTML (multiple pages)
(defun generate-html ()
  (let* ((base (asdf:component-pathname (asdf:find-system :blocks-world)))
         (output-directory (merge-pathnames \"doc/\" base)))
    (ensure-directories-exist output-directory)
    (liber:generate-html-documentation
      '(:blocks-world :blocks-world-goals)
      output-directory
      :author \"Crategus\"
      :author-url \"http://www.crategus.com\"
      :index-title \"Blocks World API documentation\"
      :heading \"blocks-world\"
      :css \"default.css\"
      :icon \"lambda.icon\"
      :single-page-p nil
      :paginate-section-p nil
      :include-slot-definitions-p t
      :include-internal-symbols-p nil)))
    @end{pre}
  @end{section}
  @begin[The Liber syntax]{section}
    Liber looks for markup tags that start with an at-sign @@, in either a long
    or a short form. The short form looks like this:
    @begin{pre}
 @@return{The string for the result.@}
    @end{pre}
    The long form can be convenient for multiple lines of text:
    @begin{pre}
 @@begin{return@}
   The string for the result.
 @@end{return@}
    @end{pre}
    The two forms are completely interchangeable. Behind the scenes, both
    produce an XML element with tag name result, @code{<result>The string for
    the result.</result>}.

    Both forms can take an optional argument, written with brackets, that is
    used in the following example to pass a hyperlink:
    @begin{pre}
 @@url[http://www.crategus.com/books/liber]{Liber API documentation@}
    @end{pre}
    The long form is:
    @begin{pre}
@@begin[http://www.crategus.com/books/liber]{url@}
  Liber API documentation
@@end{url@}
    @end{pre}
    These forms gets translated into
    @code{<a a=\"http://www.crategus.com/books/liber\">Liber API documentation</a>},
    until the XSLT stylesheets rename the @code{<a>} tag into @code{href}.

    A second example with an argument in brackets is:
    @begin{pre}
 @@begin[Title]{section@}
   body
 @@end{section@}
    @end{pre}
    which gets translated into @code{<section section=\"Title\">body</section>}.

    The at-sign also escapes special characters. For example, closing braces
    need to be escaped with the at-sign like @code{{n,m@@@}}.

    Multiple line breaks delimit paragraphs:
    @begin{pre}
  First paragraph.

  Second paragraph.
    @end{pre}
  @end{section}
  @begin[Writing a documentation string]{section}
    Here is an example of what the documentation of the
    @url[https://crategus.com/books/cl-cffi-gtk4/pages/gtk_fun_application-active-window.html]{gtk:application-active-window}
    accessor looks like using Liber:
    @begin{pre}
(setf (liber:alias-for-function 'application-active-window)
      \"Accessor\"
      (documentation 'application-active-window 'function)
 \"@@version{2025-07-28@}
  @@syntax{(gtk:application-active-window object) => window@}
  @@argument[object]{a @@class{gtk:application@} instance@}
  @@argument[window]{a @@class{gtk:window@} widget@}
  @@begin{short@}
    The accessor for the @@slot[gtk:application]{active-window@} slot of the
    @@class{gtk:application@} class returns the active window of the application.
  @@end{short@}
  The active window is the one that was most recently focused within the
  application. This window may not have the focus at the moment if another
  application has it - this is just the most recently focused window within
  this application.
  @@see-class{gtk:application@}
  @@see-class{gtk:window@}\")
    @end{pre}
    Note that parts of the documentation strings are just documentation text,
    which will be included in a section \"Details\" of the page. Other parts,
    however, are not part of the actual text, and will be extracted from the
    documentation string as the first step of processing it. In this case,
    @code{@@argument}, @code{@@return}, and @code{@@see-function} are the tags
    that will be removed. All @code{@@argument} tags will be collected into a
    section about the function's arguments, all @code{@@see-function} tags will
    be collected into a \"See also\" section.
    @begin[Tags for use only in the docstring of a package itself]{subsection}
      The following tags are listed in their short form, but are typically used
      in their long form.
      @begin[code]{table}
        @entry[@section[title@]{body}]{Generates a section called @code{title}
          with @code{body} as the content. A table of contents listing the
          sections will be generated at the top of the package pages.}
        @entry[@subsection[title@]{body}]{Generates a subsection called
          @code{title} with @code{body} as the content.}
      @end{table}
      The following tags inserts the corresponding symbol @code{name} and its
      short description which iss the contents of @code{@@short} in its
      docstring.
      @begin[code]{table}
        @entry[@about-symbol{name}]{Insert the name of a symbol @code{name} and
          its short description.}
        @entry[@about-class{name}]{Insert the name of class @code{name} and its
          short description.}
        @entry[@about-type{name}]{Insert the name of type @code{name} and its
          short description.}
        @entry[@about-generic{name}]{Insert the name of an generic function
          @code{name} and its short description.}
        @entry[@about-function{name}]{Insert the lambda list of function
           @code{name} and its short description.}
        @entry[@about-macro{name}]{Insert the lambda list of macro @code{name}
          and its short description.}
      @end{table}
    @end{subsection}
    @begin[Tags that will be extracted into their own sections]{subsection}
      @begin[code]{table}
        @entry[@syntax[name@]{description}]{Will be moved into the \"Syntax\"
          section.}
        @entry[@argument[name@]{description}]{Will be moved into the
          \"Arguments\" section.}
        @entry[@return{description}]{Will be moved into the \"Return Value\"
          section.}
        @entry[@see-function{name}]{Link to the function named @code{name}.
          Syntactically like @code{@@fun}, this tag will be moved into the
          \"See also\" section.}
        @entry[@see-slot{name}]{This tag specifies a slot reader function for
          the class it is used in, and will be moved into a \"Slot Access
          Functions\" sections. In addition, a section \"Inherited Slot Access
          Functions\" will be shown for subclasses.}
        @entry[@see-constructor{name}]{This tag specifies a function creating
          instances of current class, and will be moved into a \"Returned By\"
          section.}
      @end{table}
    @end{subsection}
    @begin[Tags for use in the documentation text]{subsection}
      The following tags are predefined to emphasize the use of symbols in a
      way similar to the Common Lisp Hyperspec. The fonts of these tags are
      predefined in a style sheet, which can be overwritten. Furthermore, these
      fonts are used within other tags like @code{@@argument}, @code{@@return},
      or @code{@@syntax}. This way a uniform style can be ensured over the
      generated documentation.
      @begin[code]{table}
        @entry[@code{name}]{Denotes a sample @code{name} or piece of code that
          a programmer might write in Common Lisp.}
        @entry[@sym{name}]{Denotes a symbol @code{@b{name}} in the package which
          is documented. The predefined font is @code{monospace bold}.}
        @entry[@arg{name}]{Denotes the name of a @arg{parameter} or @arg{value}
          of a lambda-list or a return value. The font is defined as
          @code{monospace italic} in a style sheet.}
        @entry[@term{name}]{Denotes a @term{formal term} which might be defined
          in a glossary.}
        @entry[@defterm{name}]{Like the above tag @code{@@term{@}} but in a bold
          font to emphasize the indroduction of a @defterm{new formal term}.}
      @end{table}
      @begin[code]{table}
        @entry[@short{text}]{Copies text into the output normally, but will also
          extract it for use with @code{@@about-function}, @code{@@about-class}
          ... }
        @entry[@code{text}]{In-line Lisp code @code{text}, will be formatted
          using a fixed-width font.}
        @entry[@url[URL@]{name}]{Hyperlink. This tag accepts an argument, the
          URL to be used as the href attribute.}
        @entry[@fun{name}]{Link to the function named @code{name}, read as a
          symbol into the current package (qualify with a package name to
          reference other packages included in the same documentation).}
        @entry[@class{name}]{Link to the class named @code{name}. Works like
          @code{@@fun}.}
        @entry[@variable{name}]{Link to the special variable named @code{name}.
          Works like @code{@@fun}.}
      @end{table}
    @end{subsection}
    @begin[Tags for Lists]{subsection}
      Use the following tags to generate unordered, ordered, or defintion lists
      in the documentation.
      @begin[code]{table}
        @entry[@itemize, @item]{An unordered list like <ul> and <li>}
        @entry[@enumerate, @item]{An ordered list like <ol> and >li>}
        @entry[@table, @entry]{A definition list like <dl>, <dt>, <dd>}
      @end{table}
    @end{subsection}
    @begin[Tags that are passed through to HTML]{subsection}
      @begin[code]{table}
        @entry[@pre{text}]{Preformatted section, e.g. for source code listings.}
        @entry[@b{text}]{Bold font.}
        @entry[@em{text}]{Italic font.}
        @entry[@br{}]{A single line break.}
        @entry[@break{}]{Two line breaks.}
      @end{table}
    @end{subsection}
  @end{section}
  @begin[Generating formatted documentation]{section}
    Generates HTML pages or a single HTML page.
    @about-function{generate-html-documentation}
  @end{section}
  @begin[Generating unformatted XML]{section}
    Power users might want to extract docstrings into XML and then
    send that XML through their own XSLT stylesheets.
    @about-function{extract-documentation}
  @end{section}")

;;; 2025-09-06
