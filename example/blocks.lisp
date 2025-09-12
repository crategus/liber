;;;; ---------------------------------------------------------------------------
;;;; Created: 10 December 1992
;;;; Copyright 1992 Patrick H. Winston and Berthold K. P. Horn.
;;;; All rights reserved.
;;;;
;;;; Version 1.0.1, copied from master file on 23 Apr 93
;;;;
;;;; This software is licensed by Patrick H. Winston and Berthold K. P. Horn
;;;; (licensors) for instructional use with the textbooks ``Lisp,'' by Patrick
;;;; H. Winston and Berthold K. P. Horn, and ``Artificial Intelligence,'' by
;;;; Patrick H. Winston.  Your are free to make copies of this software and
;;;; modify it for such instructional use as long as:
;;;; 1. You keep this notice intact.
;;;; 2. You cause any modified files to carry a prominent notice stating
;;;;    that you modified the files and the date of your modifications.
;;;; This software is licensed ``AS IS'' without warranty and the licensor
;;;; shall have no liability for any alleged defect or damages.
;;;;
;;;; Changed by David Lichteblau 2007:
;;;;   - added package.lisp, put each file into its own package
;;;;   - removed the #-clos and #+gclisp conditionalizations
;;;;   - added defvars for *HAND* and TABLE as well as B1 to L8
;;;;   - moved 21blocks.dta into this file
;;;;   - added documentation
;;;; ---------------------------------------------------------------------------

(in-package :blocks-world)

(defvar *hand*)

(defvar table nil "@unexport{}")
(defvar b1)
(defvar b2)
(defvar b3)
(defvar b4)
(defvar w5)
(defvar b6)
(defvar w7)
(defvar l8)

;;; ----------------------------------------------------------------------------

(defclass basic-block ()
  ((name :accessor block-name :initarg :name)
   (width :accessor block-width :initarg :width)
   (height :accessor block-height :initarg :height)
   (position :accessor block-position :initarg :position)
   (supported-by :accessor block-supported-by :initform nil)))

(setf (documentation 'basic-block 'type)
 "@version{2025-09-06}
  @begin{short}
    The superclass of all objects in the Blocks World (not including the hand).
  @end{short}
  Subclasses of the @class{blocks-world:basic-block} class characterize
  different kinds of objects, and have different properties.

  They all have a name, given as the @slot[blocks-world:basic-block]{name} slot
  and in the examples from the book, a global variable of that name is used to
  refer to them. Since this chapter is an explanation of CLOS, no specific
  constructor function is defined, and users may call @code{make-instance}
  directly.
  @see-slot{blocks-world:block-name}
  @see-slot{blocks-world:block-width}
  @see-slot{blocks-world:block-height}
  @see-slot{blocks-world:block-position}
  @see-slot{blocks-world:block-supported-by}")

(setf (documentation (liber:slot-documentation "name" 'basic-block) t)
 "The name of the block.")

(defgeneric block-name (instance)
  (:documentation
 "@version{2025-09-06}
  @argument[instance]{a @class{blocks-world:basic-block} instance}
  @return{The symbol for the name.}
  @short{Returns the name of the block.}
  In the examples from the book, a global variable of this name is used to
  refer to @arg{instance}.
  @see-class{blocks-world:basic-block}"))

(setf (documentation (liber:slot-documentation "width" 'basic-block) t)
 "The width of the block.")

(defgeneric block-width (instance)
  (:documentation
 "@version{2025-09-06}
  @argument[instance]{a @class{blocks-world:basic-block} instance}
  @return{The integer for the width.}
  @short{Returns the width of the block.}
  The size of a block is specified as width and height, and determines which
  parts of the world this block occupies. No other objects can be placed to an
  overlapping position.
  @see-class{blocks-world:basic-block}
  @see-function{blocks-world:block-position}
  @see-function{blocks-world:block-height}"))

(setf (documentation (liber:slot-documentation "height" 'basic-block) t)
 "The height of the block.")

(defgeneric block-height (instance)
  (:documentation
 "@version{2025-09-06}
  @argument[instance]{a @class{blocks-world:basic-block} instance}
  @return{The integer for the height.}
  @short{Returns the height of the block.}
  The size of a block is specified as width and height, and determines which
  parts of the world this block occupies. No other objects can be placed to an
  overlapping position.
  @see-class{blocks-world:basic-block}
  @see-function{blocks-world:block-position}
  @see-function{blocks-world:block-width}"))

(setf (documentation (liber:slot-documentation "position" 'basic-block) t)
 "The position of the block.")

(defgeneric block-position (instance)
  (:documentation
 "@version{2025-09-06}
  @argument[instance]{a @class{blocks-world:basic-block} instance}
  @return{The list of two integers for the position.}
  @short{Returns the position of the block.}
  The position of a block is specified as a list of its x and y coordinates,
  where the first axis runs along the table, and the second axis points upwards
  towards the hand.

  Together with the block's width and height, the position determines which
  parts of the world this block occupies. No other objects can be placed to an
  overlapping position.
  @see-class{blocks-world:basic-block}
  @see-function{blocks-world:block-height}
  @see-function{blocks-world:block-width}
  @see-function{blocks-world:hand-position}"))

(setf (documentation (liber:slot-documentation "supported-by" 'basic-block) t)
 "The block this instance has been placed onto.")

(defgeneric block-supported-by (instance)
  (:documentation
 "@version{2025-09-06}
  @argument[instance]{a @class{blocks-world:basic-block} instance}
  @return{The @class{blocks-world:basic-block} instance, or @code{nil}.}
  @short{Returns the block this instance has been placed onto.}
  All blocks except for the table sit on top of another block, which supports
  them.
  @see-class{blocks-world:basic-block}
  @see-function{blocks-world:block-support-for}"))

;;; ----------------------------------------------------------------------------

(defclass load-bearing-block (basic-block)
  ((support-for :accessor block-support-for :initform nil))
  (:documentation
 "@version{2025-09-06}
  @begin{short}
    The superclass of objects in the Blocks World that other blocks can be
    placed onto.
  @end{short}
  This class is mixed into most blocks, except for the
  @class{blocks-world:wedge} and the @class{blocks-world:ball}.
  @see-slot{blocks-world:block-support-for}
  @see-class{blocks-world:basic-block}
  @see-class{blocks-world:wedge}
  @see-class{blocks-world:ball}"))

(setf (documentation (liber:slot-documentation "support-for"
                                               'load-bearing-block) t)
 "The list of blocks that have been placed onto this instance.")

(defgeneric block-support-for (instance)
  (:documentation
 "@version{2025-09-06}
  @argument[instance]{a @class{blocks-world:load-bearing-block} instance}
  @begin{return}
    The list of @class{blocks-world:load-bearing-block} instances for the
    blocks.
  @end{return}
  @short{Returns the blocks that have been placed onto this instance.}
  @see-class{blocks-world:load-bearing-block}
  @see-function{blocks-world:block-supported-by}"))

(defmethod block-support-for ((object basic-block))
  nil)

;;; ----------------------------------------------------------------------------

(defclass movable-block (basic-block) ())

(setf (documentation 'movable-block 'type)
 "@version{2025-09-06}
  @begin{short}
    The superclass of objects in the Blocks World that can be moved by the
    hand.
  @end{short}
  This class is mixed into all blocks except for the @class{blocks-world:table}.
  @see-class{blocks-world:basic-block}
  @see-class{blocks-world:table}")

;;; ----------------------------------------------------------------------------

(defclass table (load-bearing-block) ())

(setf (documentation 'table 'type)
 "@version{2025-09-06}
  @short{The table supporting the rest of the world.}
  The entire rest of the world sits on this table. The table itself cannot be
  moved. For each world, this class is meant to be a singleton.
  @see-class{blocks-world:load-bearing-block}")

;;; ----------------------------------------------------------------------------

(defclass brick (movable-block load-bearing-block) ())

(setf (documentation 'brick 'type)
 "@version{2025-09-06}
  @short{A useful movable building block with a flat top.}
  Because this block has a flat top, it supports other blocks.
  @see-class{blocks-world:load-bearing-block}")

(defclass wedge (movable-block) ())

(setf (documentation 'wedge 'type)
 "@version{2025-09-06}
  @short{An interesting movable building block.}
  Because this block doesn't have a flat top, it cannot support other blocks.
  @see-class{blocks-world:movable-block}")

(defclass ball (movable-block) ())

(setf (documentation 'ball 'type)
 "@version{2025-09-06}
  @short{The block is a sphere.}
  Because this block does not have a flat top, it cannot support other blocks.
  @see-class{blocks-world:movable-block}")

;;; ----------------------------------------------------------------------------

(defclass hand ()
  ((name :accessor hand-name :initarg :name)
   (position :accessor hand-position :initarg :position)
   (grasping :accessor hand-grasping :initform nil)))

(setf (documentation 'hand 'type)
 "@version{2025-09-06}
  @short{The hand that moves the world.}
  This hand can be used to move every @class{blocks-world:movable-block}.
  @see-slot{blocks-world:hand-name}
  @see-slot{blocks-world:hand-position}
  @see-slot{blocks-world:hand-grasping}
  @see-class{blocks-world:movable-block}")

(defgeneric hand-name (instance)
  (:documentation
 "@version{2025-09-06}
  @argument[instance]{a @class{blocks-world:hand} instance}
  @return{The symbol for the name of @arg{instance}.}
  @short{Returns the name of the hand.}
  @begin[Implementation note]{dictionary}
    The hand is always called @code{blocks-world::*hand*}.
  @end{dictionary}
  @see-class{blocks-world:hand}"))

(defgeneric hand-position (instance)
  (:documentation
 "@argument[instance]{a @class{blocks-world:hand} instance}
  @return{The list of two integers for the position.}
  @short{Returns the position of the hand.}
  The position of a hand is specified as a list of its x and y coordinates,
  where the first axis runs along the table, and the second axis points upwards
  towards the hand.
  @see-class{blocks-world:hand}
  @see{blocks-world:block-position}"))

(defgeneric hand-grasping (instance)
  (:documentation
 "@version{2025-09-06}
  @argument[instance]{a @class{blocks-world:hand} instance}
  @return{The @class{movable-block} instance, or @code{nil}.}
  @short{Returns the block the hand is currently holding.}
  @see-class{blocks-world:hand}
  @see-class{blocks-world:movable-block}"))

;;; ----------------------------------------------------------------------------

(defgeneric put-on (instance support)
  (:documentation
 "@version{2025-09-06}
  @argument[instance]{a @class{blocks-world:movable-block} instance}
  @argument[support]{a @class{blocks-world:basic-block} instance}
  @return{@em{True} on success.}
  @short{Move block @arg{instance} onto block @arg{support}.}
  Prints the steps taken and returns @em{true} or prints an error message and
  returns @em{false}.
  @see-class{blocks-world:movable-block}
  @see-class{blocks-world:basic-block}
  @see-function{blocks-world:get-space}
  @see-function{blocks-world:grasp}
  @see-function{blocks-world:move}
  @see-function{blocks-world:ungrasp}"))

(defmethod put-on ((object movable-block) (support basic-block))
  (if (get-space object support)
      (and (grasp object)
           (move object support)
           (ungrasp object))
      (format t "~&Sorry, there is no room for ~a on ~a."
              (block-name object)
              (block-name support))))

(defgeneric get-space (instance support)
  (:documentation
 "@version{2025-09-06}
  @argument[instance]{a @class{movable-block} instance}
  @argument[support]{a @class{basic-block} instance}
  @return{Undocumented, but non-@code{nil}.}
  @short{Find or make space on @arg{support} for @arg{instance}.}
  @see-class{blocks-world:movable-block}
  @see-class{blocks-world:basic-block}
  @see-function{blocks-world:find-space}
  @see-function{blocks-world:make-space}"))

(defmethod get-space ((object movable-block) (support basic-block))
  (or (find-space object support)
      (make-space object support)))

(defgeneric grasp (instance)
  (:documentation
 "@version{2025-09-06}
  @argument[instance]{a @class{movable-block} instance}
  @return{The @em{true} value.}
  @short{Grasps the block using the hand.}
  Makes sure to ungrasp the block currently grasped by the
  @class{blocks-world:hand} instance, if any.
  @see-class{blocks-world:movable-block}
  @see-class{blocks-world:hand}
  @see-function{blocks-world:ungrasp}"))

(defmethod grasp ((object movable-block))
  (unless (eq (hand-grasping *hand*) object)
    (when (block-support-for object) (clear-top object))
    (when (hand-grasping *hand*)
      (get-rid-of (hand-grasping *hand*)))
    (format t "~&Move hand to pick up ~a at location ~a."
            (block-name object)
            (top-location object))
    (setf (hand-position *hand*) (top-location object))
    (format t "~&Grasp ~a." (block-name object))
    (setf (hand-grasping *hand*) object))
  t)

(defgeneric ungrasp (instance)
  (:documentation
 "@version{2025-09-06}
  @argument[instance]{a @class{blocks-world:movable-block} instance}
  @return{@em{True} on success.}
  @short{Ungrasps the block if hand is holding it.}
  Returns @em{true} if successful, or @em{false} if the
  @class{blocks-world:hand} instance did not hold this block.
  @see-class{blocks-world:movable-block}
  @see-class{blocks-world:hand}
  @see-fucntion{blocks-world:grasp}"))

(defmethod ungrasp ((object movable-block))
  (when (block-supported-by object)
    (format t "~&Ungrasp ~a." (block-name object))
    (setf (hand-grasping *hand*) nil)
    t))

(defgeneric get-rid-of (instance)
  (:documentation
 "@version{2025-09-06}
  @argument[instance]{a @class{movable-block} instance}
  @return{Unspecified}
  @short{Moves @arg{instance} onto the @class{blocks-world:table} instance.}
  @see-class{blocks-world:movable-block}
  @see-class{blocks-world:table}
  @see-function{blocks-world:put-on}"))

(defmethod get-rid-of ((object movable-block))
  (put-on object table))

(defgeneric make-space (instance support)
  (:documentation
 "@version{2025-09-06}
  @argument[instance]{a @class{movable-block} instance}
  @argument[support]{a @class{basic-block} instance}
  @return{Undocumented, but non-@code{nil}.}
  @short{Make space on support for object.}
  Takes all necessary actions to make space available.
  @see-class{blocks-world:movable-block}
  @see-class{blocks-world:basic-block}
  @see-function{blocks-world:get-space}
  @see-function{blocks-world:find-space}"))

(defmethod make-space ((object movable-block) (support basic-block))
  (dolist (obstruction (block-support-for support))
    (get-rid-of obstruction)
    (let ((space (find-space object support)))
      (when space (return space)))))

(defgeneric clear-top (instance)
  (:documentation
 "@version{2025-09-06}
  @argument[instance]{a @class{load-bearing-block} instance}
  @return{@em{False}}
  @short{Make space on top of this instance.}
  Removes all blocks @arg{instance} is supporting.
  @see-class{blocks-world:load-bearing-block}
  @see-function{blocks-world:get-rid-of}
  @see-function{blocks-world:block-support-for}"))

(defmethod clear-top ((support load-bearing-block))
  (dolist (obstacle (block-support-for support) t)
    (get-rid-of obstacle)))

(defgeneric remove-support (instance)
  (:documentation
 "@version{2025-09-06}
  @argument[instance]{a @class{blocks-world:movable-block} instance}
  @return{@em{True}}
  @short{Note that @arg{instance} has been taken from @code{support}.}
  This function maintains the @slot[blocks-world:basic-block]{supported-by} and
  @slot[blocks-world:load-bearing-block]{support-for} slots.
  @see-class{blocks-world:movable-block}
  @see-class{blocks-world:basic-block}
  @see-class{blocks-world:load-bearing-block}"))

(defmethod remove-support ((object movable-block))
  (let ((support (block-supported-by object)))
    (when support
      (setf (block-support-for support)
            (remove object (block-support-for support)))
      (setf (block-supported-by object) nil)
      t)))

(defgeneric add-support (instance support)
  (:documentation
 "@version{2025-09-06}
  @argument[instance]{a @class{blocks-world:movable-block} intance}
  @argument[support]{a @class{blocks-world:basic-block} instance}
  @return{@em{True}}
  @short{Note that @arg{instance} has been put onto @arg{support}.}
  This function maintains the @slot[blocks-world:basic-block]{supported-by} and
  @slot[blocks-world:load-bearing-block]{block-support-for} slots.
  @see-class{blocks-world:movable-block}
  @see-class{blocks-world:basic-block}"))

(defmethod add-support ((object movable-block)
                        (support basic-block))
  t)

(defmethod add-support ((object movable-block)
                        (support load-bearing-block))
  (push object (block-support-for support))
  (setf (block-supported-by object) support)
  t)

(defgeneric move (instance support)
  (:documentation
 "@version{2025-09-06}
  @argument[instance]{a @class{blocks-world:movable-block} instance}
  @argument[support]{a @class{blocks-world:load-bearing-block} instance}
  @return{@em{True}}
  @short{Move block @arg{instance} onto block @arg{support}.}
  This is a helper function for the @fun{blocks-world:put-on} function.
  @see-class{blocks-world:movable-block}
  @see-class{blocks-world:load-bearing-block}
  @see-function{blocks-world:put-on}"))

(defmethod move :before ((object movable-block) ignored-parameter)
  (let ((support (block-supported-by object)))
    (when support
      (format t "~%Removing support relations between ~a and ~a."
          (block-name object) (block-name support))
      (setf (block-support-for support)
            (remove object (block-support-for support)))
      (setf (block-supported-by object) nil)
      t)))

(defmethod move :after ((object movable-block)
                        (support load-bearing-block))
  (format t "~%Adding support relations between ~a and ~a."
          (block-name object) (block-name support))
  (setf (block-support-for support)
        (cons object (block-support-for support)))
  (setf (block-supported-by object) support)
  t)

(defmethod move ((object movable-block) (support basic-block))
  (let ((newplace (get-space object support)))
    (format t "~&Move ~a to top of ~a at location ~a."
            (block-name object)
            (block-name support)
            newplace)
    (setf (block-position object) newplace))
  t)

(defmethod (setf block-position)
           :after
           (new-position (object movable-block))
  (setf (hand-position *hand*) (top-location object)))

(defun find-space (instance support)
 "@version{2025-09-06}
  @argument[instance]{a @class{blocks-world:movable-block} instance}
  @argument[support]{a @class{blocks-world:basic-block} instance}
  @return{Undocumented or @code{nil}.}
  @short{Find space on @arg{support} for @arg{instance}.}
  Returns @code{nil} if no space could be found.
  @see-class{blocks-world:movable-block}
  @see-class{blocks-world:basic-block}
  @see-function{blocks-world:get-space}
  @see-function{blocks-world:make-space}"
  (dotimes (offset (+ 1 (- (block-width support)
                           (block-width instance))))
    (unless (intersections-p instance offset
                             (first (block-position support))
                             (block-support-for support))
      (return (list (+ offset (first (block-position support)))
                    (+ (second (block-position support))
                       (block-height support)))))))

(defun intersections-p (object offset base obstacles)
  (dolist (obstacle obstacles)
    (let* ((ls-proposed (+ offset base))
           (rs-proposed (+ ls-proposed (block-width object)))
           (ls-obstacle (first (block-position obstacle)))
           (rs-obstacle (+ ls-obstacle (block-width obstacle))))
      (unless (or (>= ls-proposed rs-obstacle)
                  (<= rs-proposed ls-obstacle))
        (return t)))))

(defun top-location (object)
  (list (+ (first (block-position object))
           (/ (block-width object) 2))
        (+ (second (block-position object))
           (block-height object))))

(defmethod print-object ((x basic-block) stream)
  (format stream "#<block ~a>" (block-name x)))


;;;; from 21blocks.dta

;;;; Created: 8 December 1992
(defparameter *blocks*
 (list
  (make-instance 'table :name 'table :width 20 :height 0 :position '(0 0))
  (make-instance 'brick :name 'b1 :width 2 :height 2 :position '(0 0))
  (make-instance 'brick :name 'b2 :width 2 :height 2 :position '(2 0))
  (make-instance 'brick :name 'b3 :width 4 :height 4 :position '(4 0))
  (make-instance 'brick :name 'b4 :width 2 :height 2 :position '(8 0))
  (make-instance 'wedge :name 'w5 :width 2 :height 4 :position '(10 0))
  (make-instance 'brick :name 'b6 :width 4 :height 2 :position '(12 0))
  (make-instance 'wedge :name 'w7 :width 2 :height 2 :position '(16 0))
  (make-instance 'ball  :name 'l8 :width 2 :height 2 :position '(18 0))
 ))

(dolist (l *blocks*) (set (block-name l) l))

(dolist (l (remove table *blocks*))
  (pushnew l (block-support-for table))
  (setf (block-supported-by l) table))

(setf *hand* (make-instance 'hand :name '*hand* :position '(0 6)))
