;;; -*- Mode: Lisp; Package: HANK -*-
;;;
;;; Author: Stuart Watt
;;;         Open University
;;;
;;; Design: Paul Mulholland
;;;         Stuart Watt
;;;         Open University
;;;
;;; Spreadsheet interface stuff. The main stuff we have to do here is a 
;;; graph element handler for spreadsheets. Here, graph elements will have
;;; attachment points and so on, but unlike Ousel, graph elements will be 
;;; like spreadsheets. Also unlike Ousel, graph elements will need to be
;;; properly composite, so that one graph element can contain others. And
;;; to cap it all, we should be able to edit these spreadsheet elements in
;;; interesting ways. In particular, we should be able to simply click on
;;; a textual element within a spreadsheet and edit it. 
;;;
;;; SNW 25/3/97. This is a dramatically revised grid system. There is a good
;;; reason for this: grids are no longer restricted to windows, but are to become
;;; more like widgets as elements in their own right. This means we may want to
;;; put them in a graph element. 
;;;
;;; A grid needs to be drawn somewhere. This somewhere needs to be a window. But
;;; we should probably also take the layout into consideration for scaling
;;; purposes. When we're drawing the grid, this is easy. It becomes a bit harder
;;; when the grid's contents are changing dynamically. We have to do this in the
;;; context of the whole graph, basically. 

(in-package "HANK")

;;; A spreadsheet graph element is drawn as a set of rows. Each row has text
;;; in it, and falls into a number of columns. It is also possible that a 
;;; graph element has a defined body, in which case, we stick to that. 
;;; There are also a number of standard properties: a title, a set of headers,
;;; and a number of columns. 

(defclass grid-graph-element (node-element)
  ((selections 
    :initform (make-array '(0 0)
                :element-type 'bit
                :adjustable t))))

(defmethod grid-graph-element-row-height ((graph-element grid-graph-element))
  *grid-row-height*)

;;; There are three parts to drawing a grid graph element, generally. We first
;;; want to draw a header. We do this using the standard stuff. Having done
;;; this, we draw the grid itself. Then, we may well find that there is more to
;;; be drawn, if the grid element contains more grid graph elements. We'll need
;;; to handle this with a bit of care, but the principle is fairly simple. For
;;; now, though, we can stick with the simple stuff involved in a typical grid
;;; graph element. 
;;;
;;; The size of the graph element depends on the size of the grid. We need to call the
;;; function manually, therefore, whenever we feel that the area of the graph element
;;; needs to be properly adjusted. In fact, it is the area of the whole of the grid
;;; item, plus the header. 

(defmethod grid-graph-element-cells ((grid-graph-element grid-graph-element))
  '#.(cg:make-position 2 2))

(defmethod adjust-cells ((grid-graph-element grid-graph-element))
  (let ((cells (grid-graph-element-x-cells grid-graph-element)))
    (adjust-array (slot-value grid-graph-element 'selections) 
                  (list (cg:position-x cells) (cg:position-y cells))
      :initial-element 0)))

;;; Look to check that a cell exists. This is a useful
;;; and important function when we are asked to handle
;;; cells which might be out of the grid.

(defun grid-graph-element-cell-exists-p (grid-graph-element point)
  (let ((x (cg:position-x point))
        (y (cg:position-y point))
        (cells (grid-graph-element-cells grid-graph-element)))
    (and (pro:i>= x 0)
         (pro:i>= y 0)
         (pro:i< x (cg:position-x cells))
         (pro:i< y (cg:position-y cells)))))

;;; We can test to see whether a cell is selected or not
;;; simply by looking it up in the selection table. We
;;; can convert it to a boolean at the same time.

(defun grid-graph-element-cell-selected-p (grid-graph-element point)
  (pro:i= (bit (slot-value grid-graph-element 'selections) 
               (cg:position-x point) (cg:position-y point)) 1))

(defmethod grid-graph-element-cell-data ((grid-graph-element grid-graph-element) point)
  ())

;;; Setting a grid is similarly straightforward, except that we
;;; should draw the cell if drawing is enabled. It usually is.
;;; Only draw the cell again if the selection state has changed
;;; since it was last drawn. 

(defun (setf grid-graph-element-cell-selected-p) (value grid-graph-element point)
  (let* ((selection (slot-value grid-graph-element 'selections))
         (x (cg:position-x point))
         (y (cg:position-y point))
         (old-value (bit selection x y))
         (new-value (if value 1 0)))
    (setf (bit selection x y) new-value)
    value))

;;; Some high level functions which are useful in various
;;; circumstances, particularly for the user and for
;;; implementing the higher level user interface.

(defun ngrid-graph-element-next-cell (grid-graph-element point &key (nextp t) (test #'grid-graph-element-cell-selected-p))
  (let* ((x (cg:position-x point))
         (y (cg:position-y point))
         (cells (grid-graph-element-cells grid-graph-element))
         (x-length (cg:position-x cells))
         (y-length (cg:position-y cells)))
    (do ((y y (pro:i1+ y))
         (first-row-p t nil))
        ((pro:i>= y y-length))
      (do ((x (if first-row-p x 0) (pro:i1+ x)))
          ((pro:i>= x x-length))
        (cg:nmake-position point x y)
	       (when (and (null nextp) (funcall test grid-graph-element point))
          (return-from ngrid-graph-element-next-cell t))
        (setf nextp nil))))
  nil)

;;; To deselect all the cells in the grid, simply go through
;;; with a given point deselecting cells until there are
;;; no more selected cells.

(defun grid-graph-element-deselect-all (grid-graph-element &key (test #'grid-graph-element-cell-selected-p))
  (let ((point '#.(cg:make-position 0 0))
        (firstp t))
    (cg:nmake-position point 0 0)
    (loop
      (unless (ngrid-graph-element-next-cell grid-graph-element point :nextp (not firstp) :test test)
	       (return-from grid-graph-element-deselect-all))
      (setf (grid-graph-element-cell-selected-p grid-graph-element point) nil)
      (setf firstp nil))))

(defun grid-graph-element-select-all (grid-graph-element
                                      &key (test #'(lambda (grid-graph-element cell)
                                                     (not (grid-graph-element-cell-selected-p grid-graph-element cell)))))
  (let ((point '#.(cg:make-position 0 0))
        (firstp t))
    (cg:nmake-position point 0 0)
    (loop
      (unless (ngrid-graph-element-next-cell grid-graph-element point :nextp (not firstp) :test test)
        (return-from grid-graph-element-select-all))
      (setf (grid-graph-element-cell-selected-p grid-graph-element point) t)
      (setf firstp nil))))

;;; Selecting a cell is much easier, but not when we take
;;; into account dragging. Then, everything becomes rather
;;; complicated.
;;;
;;; ngrid-selection-bounds returns a rectangle describing the
;;; bounds of the selection. It is used principally when
;;; implementing the balance beam used to handle selection
;;; toggling. 

(defun ngrid-graph-element-selection-bounds (grid-graph-element box)
  (let* ((cells (grid-graph-element-cells grid-graph-element))
         (x-length (cg:position-x cells))
         (y-length (cg:position-y cells))
         (left most-positive-fixnum)
         (top most-positive-fixnum)
         (right most-negative-fixnum)
         (bottom most-negative-fixnum)
         (selections (slot-value grid-graph-element 'selections)))
    (do ((y 0 (pro:i1+ y)))
        ((pro:i>= y y-length))
      (do ((x 0 (pro:i1+ x)))
          ((pro:i>= x x-length))
        (when (pro:i= 1 (bit selections x y))
          (setf left (min left x))
          (setf right (max right x))
          (setf top (min top y))
          (setf bottom (max bottom y)))))
    (cg:nmake-box box left top right bottom)
    box))

;;; When we are handling changes in range selection we have
;;; a little feature which allows us to restore the selections
;;; of those which have been changed during tracking.
;;;
;;; We do this by remembering all the points whose values we
;;; haven't changed, and when we attempt to restore them we can
;;; decide what to do. This is all implemented in the tracking
;;; function which takes a point and tracks a box from that
;;; point. This requires a shared context which is implemented
;;; by the lexical closure which implements dragging.
;;;
;;; It is better to remember the values of those we haven't changed
;;; because when we are normally dragging this list will be empty. 
;;; If the point isn't in a grid cell, this should return nil, 
;;; otherwise it should return the cell. 

(defun nget-point-cell (grid-graph-element point)
  (let ((box '#.(cg:make-box 0 0 0 0))
        (x (cg:position-x point))
        (y (cg:position-y point))
        (cells (grid-graph-element-cells grid-graph-element)))
    (cg:nmake-box box x y x y)
    (nmake-grid-box-from-box grid-graph-element box)
    (if (and (pro:i/= (cg:box-left box) (cg:box-right box))
             (pro:i/= (cg:box-top box) (cg:box-bottom box))
             (pro:i<= (cg:box-left box) (cg:position-x cells))
             (pro:i<= (cg:box-top box) (cg:position-y cells)))
        (cg:nmake-position point (cg:box-left box) (cg:box-top box))
        nil)))

;;; Each time we get a new point we should look at the last grid
;;; cell we were in. If the two are different then we should start
;;; from the drag origin, and go through all the cells which
;;; are different from the last time. Everything which is new
;;; should be added, saving if required. Everything which is
;;; old should be restored if required. This should result in
;;; the right behaviour according to the guidelines. We are
;;; still missing the location cursor as distinct from the
;;; selection range, but I'm afraid I stind it too confusing
;;; to want to add it.
;;;
;;; While we are tracking, every time we get a new box we should
;;; work out the difference from the previous box. This will arrive
;;; as a number of boxes. For each box within any of these boxes
;;; (which must be normalised) we look at each point and decide
;;; whether it is in the old box or the new, and then work out
;;; what to do with the cell, whether its selection is to be changed
;;; or not.

(defun funcall-grid-cells (function grid-graph-element box &rest args)
  (let ((left (cg:box-left box))
        (top (cg:box-top box))
        (right (cg:box-right box))
        (bottom (cg:box-bottom box))
        (point '#.(cg:make-position 0 0)))
    (do ((y top (pro:i1+ y)))
	       ((pro:i>= y bottom))
      (do ((x left (pro:i1+ x)))
	         ((pro:i>= x right))
	       (cg:nmake-position point x y)
        (apply function grid-graph-element point args)))))

;;; Adding stuff to support adding and deleting rows and columns 
;;; as far as possible efficiently from the graphical point of
;;; view. This might then be used by other bits of the grid system
;;; when rows and columns need to be changed dynamically.
;;;
;;; Aargh! Need to shufffle all the selection flags in the 
;;; array to take account of the movement. This is a kind of
;;; a bitblt, so we should do something like a scroll-box, but
;;; leaving everything set to zero where new. Do this with 
;;; care to handle the overlaps between the arrays. 

(defun copy-bit-array (bits box offset)
  (let* ((dx (cg:position-x offset))
         (dy (cg:position-y offset))
         (width (pro:i- (cg:box-width box) (pro:iabs dx)))
         (height (pro:i- (cg:box-height box) (pro:iabs dy)))
         (step-x (if (pro:iplusp dx) -1 1))
         (step-y (if (pro:iplusp dy) -1 1)))
    (do ((src-x 
           (pro:i- (if (pro:iplusp dx) (pro:i1- (cg:box-right box)) (cg:box-left box)) dx)
           (pro:i+ src-x step-x))
         (count-x width (pro:i1- count-x)))
        ((pro:izerop count-x))
      (do ((src-y
             (pro:i- (if (pro:iplusp dy) (pro:i1- (cg:box-bottom box)) (cg:box-top box)) dy)
             (pro:i+ src-y step-y))
           (count-y height (pro:i1- count-y)))
          ((pro:izerop count-y))
        (setf (bit bits (pro:i+ src-x dx) (pro:i+ src-y dy)) (bit bits src-x src-y))))
     bits))

;;; These functions translate between grid and pixel coordinate systems. They are
;;; the only parts of the system that use the x and y sizes. We can, therefore, 
;;; translate this system a little bit better, using direct mapping when and 
;;; where possible. These manage the coordinate systems directly. With this
;;; strategy, we can dispense with the vectors of sizes and generate the equivalent
;;; behaviour immediately. 

(defmethod nmake-grid-box-from-box ((grid-graph-element grid-graph-element) box)
  (let ((x-size 100)
        (y-size 20)
        (cells (grid-graph-element-cells grid-graph-element)))
    (cg:nmake-box box
      (pro:i/ (cg:box-left box) x-size)
      (pro:i/ (cg:box-top box) y-size)
      (pro:imin (cg:position-x cells) (pro:i/ (pro:i+ (cg:box-right box) (pro:i1- x-size)) x-size))
      (pro:imin (cg:position-y cells) (pro:i/ (pro:i+ (cg:box-bottom box) (pro:i1- y-size)) y-size)))))

(defmethod nmake-box-from-grid-box ((grid-graph-element grid-graph-element) box)
  (let ((x-size 100)
        (y-size 20))
    (cg:nmake-box box
      (pro:i* (cg:box-left box) x-size)
      (pro:i* (cg:box-top box) y-size)
      (pro:i* (pro:i1+ (cg:box-right box)) x-size)
      (pro:i* (pro:i1+ (cg:box-bottom box)) y-size))))

(defmethod grid-graph-element-cell-data ((grid-graph-element grid-graph-element) point)
  (format () "~D,~D" (cg:position-x point) (cg:position-y point)))

(defmethod draw-graph-element ((element grid-graph-element) window redraw-box highlightp)
  (let ((box (cg:ncopy-box '#.(cg:make-box 0 0 0 0) (graph-element-area element)))
        (cell-box '#.(cg:make-box 0 0 0 0))
        (origin '#.(cg:make-position 0 0)))
    (cg:draw-box window box)
    (nbox-intersect redraw-box box)
    (cg:nmake-position origin (cg:box-left box) (cg:box-top box))
    (cg:nposition* origin -1 -1)
    (cg:ncopy-box cell-box redraw-box)
    (cg:nbox-move cell-box origin)
    (cg:nposition* origin -1 -1)
    (nmake-grid-box-from-box element cell-box)
    (funcall-grid-cells #'(lambda (element cell window)
                            (draw-grid-graph-element-cell element window cell))
                        element cell-box window)))

(defmethod draw-grid-graph-element-cell ((element grid-graph-element) window cell)
  (let ((data (grid-graph-element-cell-data element cell))
        (area (graph-element-area element))
        (box (cg:nmake-box '#.(cg:make-box 0 0 0 0) (cg:position-x cell) (cg:position-y cell) (cg:position-x cell) (cg:position-y cell)))
        (origin '#.(cg:make-position 0 0))
        left top)
    (nmake-box-from-grid-box element box)
    (cg:nmake-position origin (cg:box-left area) (cg:box-top area))
    (cg:nbox-move box origin)
    (cg:draw-box window box)
    (setf left (pro:i+ (cg:box-left box) 1))
    (setf top (pro:i+ (cg:box-top box) 1))
    (cg:move-to-x-y window left top)
    (write-string data window)))

(defmethod calculate-grid-graph-element-area ((element grid-graph-element))
  (nmake-box-from-grid-box element
    (cg:nmake-box-from-corners '#.(cg:make-box 0 0 0 0) '#.(cg:make-position 0 0)
      (grid-graph-element-cells element))))

(defun grid-graph-element-cell-box (element cell)
  (let* ((grid-box (cg:nmake-box-from-corners '#.(cg:make-box 0 0 0 0) cell cell))
         (area (graph-element-area element))
         (offset (cg:nmake-position '#.(cg:make-position 0 0) (cg:box-left area) (cg:box-top area))))
    (nmake-box-from-grid-box element grid-box)
    (cg:nbox-move grid-box offset)))

;;; Grids need a better selection system. We're going to go for a slightly
;;; more sophisticated selection system which handles dragging slightly
;;; differently. Basically, dragging in the body of a grid should drag a 
;;; selection. Only when there's a single element left should we cancel
;;; the selection. And if we select a new grid all the selections should be
;;; turned off. Eventually, we can be a bit more sophisticated, but not quite
;;; yet. In practice, we'll make the behaviour as much like a spreadsheet as
;;; we can. 

(defmethod graph-drag-handler ((window graph) (element grid-graph-element) (part cg:position) buttons position)
  (grid-drag-cell window element part buttons position))

(defmethod grid-drag-cell ((window graph) (element grid-graph-element) part buttons position)
  (declare (ignore window element part buttons position)))

