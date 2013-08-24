;;; -*- Mode: Lisp; Package: GRAPH -*-
;;;
;;; A rewritten graphs package. This time we get it to write to PostScript 
;;; files which can be read and used in documents. We don't need a full 
;;; Common Graphics driver for PostScript, although that would be nice. 
;;; Instead, we just implement the features that we actually need.

(defpackage "GRAPH"
  (:use "COMMON-LISP")
  (:export "GRAPH" "LINEAR-AXIS" "GET-COORDINATES" "MAKE-BOX"
           "MOVE-TO" "LINE-TO" "SET-LINE-WIDTH" "WITH-OPEN-GRAPH"
           "WITH-OPEN-DRAWING" "NEWPATH" "STROKE"
           "SHOW-BIT-ARRAY" "MAKE-BIT-ARRAY" "BIT-ARRAY-BIT"
           "MAKE-LINE-PROJECTION" "MAKE-TRANSLATION" "MAKE-SCALE" 
           "APPLY-TRANSFORMATION" "M*"))

(in-package "GRAPH")

(defclass axis ()
  ((maximum-value
     :initarg :maximum-value
     :accessor axis-maximum-value)
   (minimum-value
     :initarg :minimum-value
     :accessor axis-minimum-value)
   (maximum-coordinate
     :reader axis-maximum-coordinate)
   (minimum-coordinate
     :reader axis-minimum-coordinate)
   (draw
     :initarg :draw
     :initform nil
     :reader axis-draw)
   (displays
     :initarg :displays
     :initform nil
     :reader axis-displays)))

(defgeneric get-coordinate (graph axis value))

(defclass drawing ()
  ((stream
     :reader drawing-stream))
  (:default-initargs :allow-other-keys t))

(defmethod drawing-format ((drawing drawing) string &rest arguments)
  (apply #'format (drawing-stream drawing) string arguments))

(defmethod drawing-write-string (string (drawing drawing))
  (write-string string (drawing-stream drawing)))

(defun open-drawing (class pathname &rest options)
  (setf options `(:pathname ,pathname ,@options))
  (let ((drawing (apply #'make-instance class options)))
    (drawing-open drawing options)
    drawing))

(defun close-drawing (drawing &key abort)
  (drawing-close drawing abort))

(defmethod drawing-open ((drawing drawing) options)
  (let* ((pathname (getf options :pathname))
         (stream (open pathname :direction :output :if-exists :supersede)))
    (setf (slot-value drawing 'stream) stream)
    drawing))

(defmethod drawing-close ((drawing drawing) abort)
  (close (drawing-stream drawing) :abort abort))

(defmacro with-open-drawing ((drawing class pathname &rest options) &body body)
  (let ((success (gensym)))
    `(let ((,success nil)
           (,drawing (open-drawing ',class ,pathname ,@options)))
       (unwind-protect (prog1 (progn ,@body) (setq ,success t))
         (close-drawing ,drawing :abort (not ,success))))))

(defmacro with-open-graph ((graph pathname &rest options) &body body)
  `(with-open-drawing (,graph graph ,pathname ,@options)
     ,@body))

(defclass graph (drawing)
  ((horizontal-axis
     :initarg :horizontal-axis
     :accessor graph-horizontal-axis)
   (vertical-axis
     :initarg :vertical-axis
     :accessor graph-vertical-axis)
   (current-text-height
     :initform nil
     :reader graph-current-text-height)))

(defmethod drawing-open ((graph graph) options)
  (call-next-method)
  (graph-initialise-file graph options)
  (graph-initialise-axes graph options))

(defmethod drawing-close ((graph graph) abort)
  (unless abort
    (graph-draw-axes graph)
    (drawing-format graph "%%Trailer~%")
    (drawing-format graph "end~%")
    (drawing-format graph "%%EOF~%"))
  (call-next-method))

;;; Ticks come in a variety of forms. They can either be centred or above or 
;;; a below an axis. There may be a whole bunch of tick descriptors.

(defclass axis-display ()
  ((axis
     :initarg :axis
     :reader axis-display-axis)
   (start
     :initarg :start
     :reader axis-display-start)
   (end
     :initarg :end
     :reader axis-display-end)
   (step 
     :initarg :step
     :reader axis-display-step)))

(defclass tick-display (axis-display)
  ((position
     :initform :bottom
     :initarg :position
     :reader tick-display-position)
   (size
     :initform 5
     :initarg :size
     :reader tick-display-size)))

(defclass label-display (axis-display)
  ((position
     :initform :bottom
     :initarg :position
     :reader label-display-position)
   (offset
     :initform 5
     :initarg :offset
     :reader label-display-offset)
   (format
     :initform "~,1F"
     :initarg :format
     :reader label-display-format)))

;;; Define boxes as lists. These are easy to be defined from the caller so 
;;; that we don't need to make much of this external. 

(defstruct (box (:type list)
                (:constructor make-box (left bottom width height)))
  left
  bottom
  width
  height)

(defun box-right (box)
  (+ (box-left box) (box-width box)))

(defun box-top (box)
  (+ (box-bottom box) (box-height box)))

;;; Much of the stuff for drawing allows people to move to a particular 
;;; coordinate and then we can act on them with PostScript operators.
;;;
;;; The sixes here mean that we have five significant figures. This is enough 
;;; for accuracy to a 1000 dpi printer within an A4 page. This should be 
;;; adequate and keeps big plots helpfully small. 

(defmethod newpath ((graph graph))
  (drawing-format graph "newpath~%"))

(defmethod stroke ((graph graph))
  (drawing-format graph "stroke~%"))

(defmethod move-to ((graph graph) x y)
  (multiple-value-bind (x y) (get-coordinates graph x y)
    (drawing-format graph "~6F ~6F moveto~%" x y)))
    
(defmethod line-to ((graph graph) x y)
  (multiple-value-bind (x y) (get-coordinates graph x y)
    (drawing-format graph "~6F ~6F lineto~%" x y)))

(defmethod set-line-width ((graph graph) width)
  (drawing-format graph "~D setlinewidth~%" width))

;;; When we open a graph device we should write some stuff out to signal that 
;;; this is a PostScript file. 
;;; 
;;; Perhaps the most complex bit is the initialisation of the axes 
;;; coordinates within a box specified for the graph. To make this easy we 
;;; just let the coordinates be specified direct by the graph class through 
;;; some required initialisation arguments.

(defvar graph-prolog "%%BeginProcSet: CommonLispGraph_header 1 0
48 dict begin
/WordBreak ( ) def
/DrawStringInRectangle {
  /vjustify exch def
  /hjustify exch def
  /lineheight exch def
  /top exch def
  /right exch def
  /bottom exch def
  /left exch def
  /textstring exch def
  /width right left sub def
  /height top bottom sub def
  /BreakIntoLines {
    /proc exch def
    /linewidth exch def
    /textstring exch def
    /breakwidth WordBreak stringwidth pop def
    /currentwidth 0 def
    /lastwordbreak 0 def
    /startchar 0 def
    /restoftext textstring def
    { restoftext WordBreak search
      { /nextword exch def pop
        /restoftext exch def
        /wordwidth nextword stringwidth pop def
        currentwidth wordwidth add linewidth gt
        { textstring startchar lastwordbreak startchar sub getinterval 
          proc
          /startchar lastwordbreak def
          /currentwidth wordwidth breakwidth add def }
        { /currentwidth currentwidth wordwidth add
          breakwidth add def }
        ifelse
        /lastwordbreak lastwordbreak nextword length add 1 add def }
      { pop exit }
      ifelse }
    loop
    /lastchar textstring length def
    textstring startchar lastchar startchar sub getinterval proc } def
  /LineCount {
    /lines 0 def
    { pop /lines lines 1 add def } BreakIntoLines
    lines } def
  /StringTrim {
    /thestring exch def
    /textindex thestring length 1 sub def
    { thestring textindex get 32 ne { exit } if textindex 0 eq {exit} if
      /textindex textindex 1 sub def } loop
    thestring 0 textindex 1 add getinterval } def
  /textheight textstring width LineCount lineheight mul def
  /yposition top height textheight sub vjustify mul sub lineheight sub def
  textstring width
  { StringTrim /linestring exch def
    /drawwidth linestring stringwidth pop def
    left width drawwidth sub hjustify mul add yposition moveto linestring show
    /yposition yposition lineheight sub def }
  BreakIntoLines } def
/concatprocs
  { /proc2 exch cvlit def
    /proc1 exch cvlit def
    /newproc proc1 length proc2 length add array def
    newproc 0 proc1 putinterval
    newproc proc1 length proc2 putinterval
    newproc cvx } def
/graphimage
  { /py exch def
    /px exch def
    /picstr px 8 div ceiling cvi string def
    gsave
    translate scale
    { 1 exch sub } currenttransfer concatprocs settransfer
    px py 1 [px 0 0 py neg 0 py]
    { currentfile picstr readhexstring pop } image
    grestore 
  } def
%%EndProcSet
")

(defun graph-initialise-file (graph options)
  (drawing-format graph "%!PS-Adobe-3.0 EPSF-3.0~%")
  (let ((box (getf options :bounding-box)))
    (drawing-format graph "%%BoundingBox: ~D ~D ~D ~D~%" 
      (box-left box) (box-bottom box) (box-right box) (box-top box))
    (drawing-format graph "%%Creator: SNW's Common Lisp graph package~%")
    (drawing-format graph "%%EndComments~%")
    (drawing-write-string graph-prolog graph)))

(defun set-axis-coordinates (axis minimum maximum)
  (with-slots (minimum-coordinate maximum-coordinate) axis
    (setf minimum-coordinate minimum)
    (setf maximum-coordinate maximum)))

(defun graph-initialise-axes (graph options)
  (let* ((box (getf options :box))
         (horizontal (getf options :horizontal-axis))
         (vertical (getf options :vertical-axis)))
    (with-slots (horizontal-axis vertical-axis) graph
      (setf horizontal-axis horizontal)
      (setf vertical-axis vertical))
    (set-axis-coordinates horizontal (box-left box) (box-right box))
    (set-axis-coordinates vertical (box-bottom box) (box-top box))))

;;; Axes can be drawn by providing a descriptor which specifies a set of 
;;; positions as to where that axis should be drawn. :top and :bottom 
;;; correspond to the top and bottom of the other axis, although a value can 
;;; also be used. 

(defun graph-draw-axis-line (graph x1 y1 x2 y2)
  (newpath graph)
  (move-to graph x1 y1)
  (line-to graph x2 y2)
  (stroke graph))

(defmethod axis-position ((axis axis) (position number))
  (declare (ignore axis))
  position)

(defmethod axis-position ((axis axis) (position (eql :top)))
  (declare (ignore position))
  (axis-maximum-value axis))

(defmethod axis-position ((axis axis) (position (eql :bottom)))
  (declare (ignore position))
  (axis-minimum-value axis))

(defmethod graph-draw-horizontal-axis ((graph graph) (axis axis) position vertical-axis)
  (let ((position (axis-position vertical-axis position)))
    (graph-draw-axis-line graph
      (axis-minimum-value axis) position
      (axis-maximum-value axis) position)))

(defmethod graph-draw-vertical-axis ((graph graph) (axis axis) position horizontal-axis)
  (let ((position (axis-position horizontal-axis position)))
    (graph-draw-axis-line graph
      position (axis-minimum-value axis)
      position (axis-maximum-value axis))))

;;; Drawing ticks uses all these different parameters to manage the drawing 
;;; of the actual tick values. Note that the same axis may have several tick 
;;; patterns on it. 

(defmethod graph-draw-vertical-axis-display ((graph graph) (axis axis) (display axis-display))
  (let ((start (axis-display-start display))
        (end (axis-display-end display))
        (step (axis-display-step display)))
    (do ((y start (+ y step)))
        ((> y end))
      (graph-draw-vertical-axis-display-element graph axis display y))))

(defmethod graph-draw-horizontal-axis-display ((graph graph) (axis axis) (display axis-display))
  (let ((start (axis-display-start display))
        (end (axis-display-end display))
        (step (axis-display-step display)))
    (do ((x start (+ x step)))
        ((> x end))
      (graph-draw-horizontal-axis-display-element graph axis display x))))

(defmethod graph-draw-vertical-axis-display-element ((graph graph) (axis axis) (display tick-display) y)
  (let ((position (axis-position axis (axis-display-axis display)))
        (size (tick-display-size display)))
    (multiple-value-bind (x y) (get-coordinates graph position y)
      (incf x (* size (justify-proportion (tick-display-position display))))
      (drawing-format graph "newpath ~6F ~6F moveto ~6F 0 rlineto stroke~%" x y (- size)))))

(defmethod graph-draw-vertical-axis-display-element ((graph graph) (axis axis) (display label-display) y)
  (let ((position (axis-position axis (axis-display-axis display)))
        (string (format nil (label-display-format display) y))
        (hjustify (label-display-position display)))
    (multiple-value-bind (x y) (get-coordinates graph position y)
      (incf x (label-display-offset display))
      (let ((box (ccase hjustify
                   (:top (setf hjustify :bottom) (make-box x y 50 0))
                   (:bottom (setf hjustify :top) (make-box (- x 50) y 50 0)))))
         (draw-text-in-box graph string box hjustify :centre)))))

(defmethod graph-draw-horizontal-axis-display-element ((graph graph) (axis axis) (display tick-display) x)
  (let ((position (axis-position axis (axis-display-axis display)))
        (size (tick-display-size display)))
    (multiple-value-bind (x y) (get-coordinates graph x position)
      (incf y (* size (justify-proportion (tick-display-position display))))
      (drawing-format graph "newpath ~6F ~6F moveto 0 ~6F rlineto stroke~%" x y (- size)))))

(defmethod graph-draw-horizontal-axis-display-element ((graph graph) (axis axis) (display label-display) x)
  (let ((position (axis-position axis (axis-display-axis display)))
        (string (format nil (label-display-format display) x))
        (vjustify (label-display-position display)))
    (multiple-value-bind (x y) (get-coordinates graph x position)
      (incf y (label-display-offset display))
      (let ((box (ccase vjustify
                   (:top (setf vjustify :bottom) (make-box (- x 25) y 50 0))
                   (:bottom (setf vjustify :top) (make-box (- x 25) y 50 0)))))
         (draw-text-in-box graph string box :centre vjustify)))))

(defmethod graph-draw-axes ((graph graph))
  (let* ((horizontal-axis (graph-horizontal-axis graph))
         (vertical-axis (graph-vertical-axis graph)))
    (map nil #'(lambda (position)
                 (graph-draw-horizontal-axis graph horizontal-axis position vertical-axis))
         (axis-draw horizontal-axis))
    (map nil #'(lambda (display)
                 (graph-draw-horizontal-axis-display graph horizontal-axis display))
         (axis-displays horizontal-axis))
    (map nil #'(lambda (position)
                 (graph-draw-vertical-axis graph vertical-axis position horizontal-axis))
         (axis-draw vertical-axis))
    (map nil #'(lambda (display)
                 (graph-draw-vertical-axis-display graph vertical-axis display))
         (axis-displays vertical-axis))))

;;; Now we can get on with the key graph drawing functions. These mostly 
;;; depend on the coordinate system managed by the axis classes.

(defmethod get-coordinates ((graph graph) x y)
  (values (get-coordinate graph (graph-horizontal-axis graph) x)
          (get-coordinate graph (graph-vertical-axis graph) y)))
    
;;; The coordinate methods for the linear axis. Most axes, of course, will be 
;;; linear. 
    
(defclass linear-axis (axis)
  ())

(defmethod get-coordinate ((graph graph) (axis linear-axis) value)
  (let ((maximum-coordinate (axis-maximum-coordinate axis))
        (minimum-coordinate (axis-minimum-coordinate axis))
        (maximum-value (axis-maximum-value axis))
        (minimum-value (axis-minimum-value axis)))
    (+ minimum-coordinate 
       (* (- maximum-coordinate minimum-coordinate)
          (/ (- value minimum-value)
             (- maximum-value minimum-value))))))

;;; And to convert a box, use the following procedure which converts it as it 
;;; was two different points.

(defun get-coordinate-box (graph box)
  (multiple-value-bind (cleft ctop) (get-coordinates graph (box-left box) (box-top box))
    (multiple-value-bind (cright cbottom) (get-coordinates graph (box-right box) (box-bottom box))
      (make-box cleft ctop (- cright cleft) (- cbottom ctop)))))

;;; Drawing text means that we should be able to remember the font size and 
;;; stuff and drawing text in the current font. We do this by allowing people 
;;; to draw text in a value box. Within a box of values a string can be drawn 
;;; with a variety of different justifications. 

(defun set-font (graph font-name size)
  (drawing-format graph "/~A findfont ~D scalefont setfont~%" font-name size)
  (with-slots (current-text-height) graph
    (setf current-text-height size)))

(defun justify-proportion (justify)
  (ccase justify
    ((:left :bottom) 0.0)
    (:centre 0.5)
    ((:right :top) 1.0)))

(defun draw-text-in-box (graph string box hjustify vjustify)
  (let ((hjustify (justify-proportion hjustify))
        (vjustify (justify-proportion vjustify))
        (text-height (graph-current-text-height graph)))
    (unless text-height
      (warn "Choosing default font: 12 point Times")
      (set-font graph "Times-Roman" 12)
      (setf text-height (graph-current-text-height graph)))
    (drawing-format graph "(~A) ~A ~A ~A ~A ~A ~A ~A DrawStringInRectangle~%"
      string 
      (box-left box) (box-bottom box) (box-right box) (box-top box)
      text-height hjustify vjustify)))

;;; We should be able to generate the axis labelling and ticking 
;;; automatically as far as we can. This can be done by a simple function 
;;; taking a bunch of arguments. Of course, it is possible to override the 
;;; whole thing anyway. 
;;;
;;; Ticks and labels are limited to being powers of ten, 2 times, and 5 
;;; times. This seems to provide the right kind of clean axis labelling. 
;;; Our basic rule is that there should be between 4 and 8 labelled items. 
;;; We also make sure that our labelling works. 

(defun best-value-in-interval (from to)
  (declare (ignore from to))
  ())

(defun make-axis (from to &key)
  (assert (< from to) () "The axis must have a finite extent")
  (let ((distance (- to from)))
    (declare (ignore distance))
    ()))

#||

;;; The next stage is to get axis drawing together so that boxes and lines 
;;; can be used to delimit the actual drawing area. This is actually easy to 
;;; do if we just want simple text, but it is far from easy when we want to 
;;; put ticks and labels on these axes. 

;;; Most other stuff we can do just by writing stuff to the graph. 

(defun test-ueda (&optional (pathname #P"ueda.eps") &aux startedp)
  (with-open-graph (graph pathname
                          :vertical-axis   (make-instance 'linear-axis
                                             :maximum-value 4.0
                                             :minimum-value -4.0
                                             :draw '(:top :bottom 0.0)
                                             :displays (list (make-instance 'tick-display
                                                               :axis 0.0
                                                               :start -4.0 
                                                               :end 4.0
                                                               :step 0.25
                                                               :position :centre)
                                                             (make-instance 'label-display
                                                               :axis -8.0
                                                               :start -4.0 
                                                               :end 4.0
                                                               :step 1.0
                                                               :offset -8
                                                               :position :bottom)))
                          :horizontal-axis (make-instance 'linear-axis
                                             :maximum-value 8.0
                                             :minimum-value -8.0
                                             :draw '(:top :bottom 0.0)
                                             :displays (list (make-instance 'tick-display
                                                               :axis 0.0
                                                               :start -8.0 
                                                               :end 8.0
                                                               :step 2.0
                                                               :position :centre)))
                          :box (make-box 100 100 300 300)
                          :bounding-box (make-box 90 90 320 320))
    (iterate-second-order-differential
     #'(lambda (x y dy ddy)
         (declare (ignore ddy))
         (when startedp
           (line-to graph dy y))
         (when (zerop (mod x 1.0)) (princ x) (princ " "))
         (unless startedp
           (setf startedp t)
           (newpath graph)
           (move-to graph dy y)))
     #'(lambda (x y dy)
         (- (* 7.5 (cos x))
            (* y y y)
            (* 0.05 dy)))
     :start-x 0.0
     :start-y 3.0
     :start-dy 4.0
     :step-size 0.02
     :count 300)
    (stroke graph)))

||#

;;; Another thing we also need to be able to do is to take
;;; a bitmap and dump it down into a PostScript file so that 
;;; it appears as an image. We do this by specifying an area
;;; within the graph which will form the corners of the image
;;; and then putting out the right PostScriptey bits before we
;;; get on and draw the image itself. The image is held as a 
;;; two dimensional bit array, as created by make-array. 

(defun make-bit-array (x y)
  (make-array (list y x) :element-type 'bit
    :initial-element 0))

(defun (setf bit-array-bit) (value bit-array x y)
  (setf (bit bit-array y x) value))

(defun bit-array-bit (bit-array x y)
  (bit bit-array y x))

;;; Now, when we get to generate the display for an image, we
;;; can transform the parameters: that much is easy. What is 
;;; less easy is generating the hexadecimal image data. Make sure
;;; we generate whole numbers of digits by accumulating values in
;;; octets and then writing an appropriate textual representation.
;;; When we get to the end of the row, pad with zeros. 

(defun graph-write-nibbles (graph value)
  (drawing-format graph "~C~C" 
    (digit-char (ldb #.(byte 4 4) value) 16)
    (digit-char (ldb #.(byte 4 0) value) 16)))

(defun graph-write-bit-array (graph bit-array)
  (let ((rows (array-dimension bit-array 0))
        (columns (array-dimension bit-array 1)))
    (dotimes (row rows)
      (let ((bit 0)
            (value 0)
            (bits columns)
            (y (- rows row 1)))
        (dotimes (x bits)
          (when (= bit 8)
            (graph-write-nibbles graph value)
            (setf value 0)
            (setf bit 0))
          (setf value (+ (* value 2) (bit-array-bit bit-array x y)))
          (incf bit))
        (when (> bit 0)
          (setf value (ash value (- 8 bit)))
          (graph-write-nibbles graph value))
        (drawing-format graph "~%")))))

;;; Draw the bit array by allowing to be specified with a set of
;;; corners and all that kind of thing. 

(defmethod show-bit-array ((graph graph) bit-array box)
  (let ((coordinate-box (get-coordinate-box graph box)))
    (drawing-format graph "~A ~A ~A ~A ~A ~A graphimage~%"
      (box-width coordinate-box) (box-height coordinate-box)
      (box-left coordinate-box) (box-bottom coordinate-box)
      (array-dimension bit-array 1) (array-dimension bit-array 0))
    (graph-write-bit-array graph bit-array)))

#||
(defparameter test-array
  (make-array '(12 16) :element-type 'bit
    :initial-contents '((0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0)
                        (0 0 0 0 0 0 0 1 1 1 0 0 0 0 0 0)
                        (0 0 0 0 0 0 1 1 1 1 1 0 0 0 0 0)
                        (0 0 0 0 0 1 1 1 1 1 1 1 0 0 0 0)
                        (0 0 0 0 1 1 1 1 1 1 1 1 1 0 0 0)
                        (0 0 0 1 1 1 1 1 1 1 1 1 1 1 0 0)
                        (0 0 0 0 0 0 1 1 1 1 1 0 0 0 0 0)
                        (0 0 0 0 0 0 1 1 1 1 1 0 0 0 0 0)
                        (0 0 0 0 0 0 1 1 1 1 1 0 0 0 0 0)
                        (0 0 0 0 0 0 1 1 1 1 1 0 0 0 0 0)
                        (1 1 0 0 0 0 1 1 1 1 1 0 0 0 0 0)
                        (1 1 0 0 0 0 1 1 1 1 1 0 0 0 0 0))))

(defun test-images (pathname)
  (graph:with-open-graph (graph pathname
                            :bounding-box (graph:make-box 100 100 300 300)
                            :box (graph:make-box 100 100 300 300)
                            :vertical-axis   (make-instance 'graph:linear-axis
                                               :maximum-value 1.0
                                               :minimum-value 0.0
                                               :draw '(:top :bottom))
                            :horizontal-axis (make-instance 'graph:linear-axis
                                               :maximum-value 1.0
                                               :minimum-value 0.0
                                               :draw '(:top :bottom)))
    (show-bit-array graph test-array
      (make-box 0.0 0.0 1.0 1.0))))
||#

;;; 3D coordinate geometry stuff. We can use this to build some of the image 
;;; projections that we will need for some views.

(defun m* (m1 m2)
  (let* ((m (make-identity)))
    (dotimes (i 3)
      (dotimes (j 4)
        (setf (aref m i j) (loop for k from 0 below 3
                             sum (* (aref m1 i k) (aref m2 k j)))))
      (setf (aref m i 3) (+ (aref m1 i 3) (aref m2 i 3))))
    m))
                
(defun make-identity ()
  (make-array '(4 4)
    :initial-contents '((1 0 0 0) 
                        (0 1 0 0) 
                        (0 0 1 0) 
                        (0 0 0 1))))

(defun make-scale (sx sy sz)
  (let* ((m1 (make-identity)))
    (setf (aref m1 0 0) sx)
    (setf (aref m1 1 1) sy)
    (setf (aref m1 2 2) sz)
    m1))

(defun make-translation (dx dy dz)
  (let* ((m1 (make-identity)))
    (setf (aref m1 0 3) dx)
    (setf (aref m1 1 3) dy)
    (setf (aref m1 2 3) dz)
    m1))

(defun make-rotation (angle axis)
  (let* ((m1 (make-identity))
         (s (sin angle))
         (c (cos angle))
         (axis1 (mod (1+ axis) 3))
         (axis2 (mod (1+ axis1) 3)))
     (setf (aref m1 axis axis) 1)
     (setf (aref m1 axis axis1) 0)
     (setf (aref m1 axis axis2) 0)
     (setf (aref m1 axis1 axis) 0)
     (setf (aref m1 axis1 axis1) c)
     (setf (aref m1 axis1 axis2) (- s))
     (setf (aref m1 axis2 axis) 0)
     (setf (aref m1 axis2 axis1) s)
     (setf (aref m1 axis2 axis2) c)
     (setf (aref m1 0 3) 0)
     (setf (aref m1 1 3) 0)
     (setf (aref m1 2 3) 0)
     m1))

;;; To get a 2D point from a 3D point, apply the transformation to a 3D point
;;; and take the x and y coordinates. 

(defun apply-transformation (point m1)
  (loop for i from 0 below 2
    collect (+ (aref m1 i 3)
               (loop for j from 0 below 3
                     for value in point
                 sum (* (aref m1 i j) value)))))
    
(defun make-transformation (ex ey ez tx ty tz)
  (let* ((result (make-translation (- tx) (- ty) (- tz)))
         (dx (- ex tx))
         (dy (- ey ty))
         (dz (- ez tz))
         (distance (sqrt (+ (* dx dx) (* dy dy))))
         (distance1 (sqrt (+ (* distance distance) (* dz dz)))))
    (setf result (m* result (make-rotation (- (atan dx dy)) 2)))
    (setf result (m* result (make-rotation (- pi (atan dz distance)) 1)))
    (setf result (m* result (make-rotation (atan (* distance1 dx) (- (* dy dz))) 2)))
    (setf result (m* result (print (make-translation 0 0 distance1))))
    result))

(setf tp1 (make-transformation 100 100 100 0 0 0))
(apply-transformation '(10 10 10) tp1)

;;; Make a line projection

(defun make-line-projection (angle)
  (let ((m (make-identity))
        (s (sin angle))
        (c (cos angle)))
    (setf (aref m 0 0) (- c))
    (setf (aref m 0 1) 0)
    (setf (aref m 0 2) c)
    (setf (aref m 1 0) (- s))
    (setf (aref m 1 1) 1)
    (setf (aref m 1 2) (- s))
    (setf (aref m 2 0) 0)
    (setf (aref m 2 1) 0)
    (setf (aref m 2 2) 0)
    m))

(m* (make-translation 100 100 0) (make-identity))

(setf tp1 (m* (make-translation 100 100 0) (make-line-projection (/ pi 6)))) 
(apply-transformation '(0 10 0) tp1)
