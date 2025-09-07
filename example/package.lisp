(in-package :cl-user)

(defpackage :blocks-world
  (:use :common-lisp)
  (:export #:basic-block
           #:block-name
           #:block-width
           #:block-height
           #:block-position
           #:block-supported-by

           #:movable-block

           #:load-bearing-block
           #:block-support-for

           #:brick
           #:wedge
           #:ball
           #:table

           #:hand
           #:hand-name
           #:hand-position
           #:hand-grasping

           #:put-on
           #:get-space
           #:find-space
           #:make-space
           #:grasp
           #:ungrasp
           #:get-rid-of
           #:clear-top
           #:add-support
           #:remove-support
           #:move))

(setf (documentation (find-package :blocks-world) t)
 "@version{2025-09-06}
  This package contains the source code of chapter 21, @em{\"The Blocks World
  with Classes and Methods\"} from
  @url[https://en.wikipedia.org/wiki/Lisp_(book)]{Lisp (3rd edition)} by Winston
  and Horn.
  @begin[A picture of the world]{section}
    The block objects represent a world that \"looks\" like this:
    @begin{pre}
/----\\    ^    /---------\\      ^
| b4 |   /  \\  |         |     / \\
\\____/  /_w7_\\ |         |     / \\
/----\\  /----\\ |         |    /   \\  /--------\\        /^\\
| b1 |  | b2 | | b3      |    /   \\  | b6     |       (l8 )
\\____/  \\____/ \\_________/   /_w5__\\ \\________/        \\./
+-----------------------------------------------------------+
|                                                           |
+-----------------------------------------------------------+
    @end{pre}
  @end{section}
  @begin[Example]{section}
    In the initial configuration, where all blocks have been placed directly
    on the table (not shown), the @fun{blocks-world:put-on} method will move
    the objects like this:
    @begin{pre}
BLOCKS-WORLD> (put-on b1 b2)
Move hand to pick up B1 at location (1 2).
Grasp B1.
Removing support relations between B1 and TABLE.
Move B1 to top of B2 at location (2 2).
Adding support relations between B1 and B2.
Ungrasp B1.
T
    @end{pre}
  @end{section}
  @begin[The different kinds of blocks]{section}
    Movable blocks than can be moved onto load supporting blocks. Using multiple
    inheritance, there are also blocks that can do both.
    @begin[The basic-block class]{subsection}
      @about-class{basic-block}
      @about-generic{block-name}
      @about-generic{block-position}
      @about-generic{block-width}
      @about-generic{block-height}
      @about-generic{block-supported-by}
    @end{subsection}
    @begin[The load-bearing-block class]{subsection}
      @about-class{load-bearing-block}
      @about-generic{block-support-for}
    @end{subsection}
    @begin[The movable-block class]{subsection}
      @about-class{movable-block}
    @end{subsection}
  @end{section}
  @begin[Concrete block classes]{section}
    These are the blocks found in our world.
    @about-class{table}
    @about-class{brick}
    @about-class{wedge}
    @about-class{ball}
  @end{section}
  @begin[The hand]{section}
    The hand is movable. It can hold at most one block.
    @about-class{hand}
    @about-generic{hand-name}
    @about-generic{hand-position}
    @about-generic{hand-grasping}
  @end{section}
  @begin[Methods for the blocks world]{section}
    @about-generic{put-on}
    @about-generic{get-space}
    @about-generic{make-space}
    @about-function{find-space}
    @about-generic{grasp}
    @about-generic{ungrasp}
    @about-generic{get-rid-of}
    @about-generic{clear-top}
    @about-generic{add-support}
    @about-generic{remove-support}
    @about-generic{move}
  @end{section}")

(defpackage :blocks-world-goals
  (:use :blocks-world :common-lisp)
  (:export #:node
           #:node-parent
           #:node-children
           #:node-action
           #:*current-node*
           #:attach-parent
           #:attach-action
           #:define-history-method
           #:show-simple-tree
           #:find-action
           #:tell-why))

(setf (documentation (find-package :blocks-world-goals) t)
 "This package contains the source code of chapter 22, @em{\"Answering
  Questions about Goals\"} from
  @url[https://en.wikipedia.org/wiki/Lisp_(book)]{Lisp (3rd edition)} by Winston
  and Horn.
  @begin[Lots of undocumented functions]{section}
    I was too lazy to document this package, which is why all its functions
    have a big fat \"undocumented\" warning. This package's page also looks
    rather empty and sad.
  @end{section}")
