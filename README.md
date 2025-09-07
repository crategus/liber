## Liber

### General information

The `Liber` library is a  HTML documentation generation tool for Common Lisp
packages.

It extracts documentation strings written in a custom markup language and
generates either multiple HTML pages or a single HTML page for the
documentation.

The `Liber` library is a forck of the atdoc library written by David Lichteblau.
It is developped using SBCL on Ubuntu. The library runs sucessfully on Windows.

### License

The `Liber` library is licensed under the MIT license.

### Installation and Usage

The `Liber` library is ASDF-installable:
```
(asdf:load-system :liber)
```
The `Liber` library needs Closure XML, Split sequence, Xuriella XSLT,
Closer MOP, and their dependencies.

### Documentation

Documentation is available at
[Liber API documentation](https://crategus.com/books/liber/).

### Example

In general, you will write a Lisp script that loads the necessary packages for
the documentation, configures the environment and executes the HTML generation.
Below is an example from the `generate-html.lisp` file for the `BLOCKS-WORD`
example that comes with the library.
```
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
```
