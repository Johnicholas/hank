;;; -*- Mode: Lisp; Package: HANK -*-
;;;
;;; Author: Stuart Watt
;;;         Open University
;;;
;;; Design: Paul Mulholland
;;;         Stuart Watt
;;;         Open University

(in-package "HANK")

;;; Specialise the grid-graph-element with additional properties so that we can 
;;; support tables and things like that. This time we have a better graph view system
;;; so we can make the display a little bit smarter. Column widths can be used like
;;; in the old system, but we can be a bit smarter with the rest of the code. In
;;; particular, we can use data more cleanly by getting symbol values directly. 
;;;
;;; The table element's body contains both a body and a set of rows. We have to be a
;;; bit cunning with these. Both of these need to be saved, although only one may be
;;; visible at any one time. This depends on the class of the element. Note, though,
;;; that the template cannot be different between them. 

(defclass table-element (grid-graph-element)
  ((class
    :initarg :class
    :reader table-element-class)
   (row-data
    :initarg :row-data
    :initform nil
    :reader table-element-row-data)
   (body-data
    :initarg :body-data
    :initform nil
    :reader table-element-body-data)
   (body-cells
    :initarg :body-cells
    :initform (cg:make-position 0 0)
    :reader table-element-body-cells)
   (body-size
    :initarg :body-size
    :initform (cg:copy-position *minimum-body-size*)
    :reader table-element-body-size)
   (title
    :initarg :title
    :reader table-element-title)
   (status
    :initform nil
    :initarg :status
    :reader table-element-status)
   (template
    :initarg :template
    :reader table-element-template)
   (column-widths
    :initarg :column-widths
    :reader table-element-column-widths)
   (pendingp
    :reader table-element-pending-p
    :initarg :pendingp
    :initform nil)
   (linkablep
    :initform t
    :initarg :linkablep
    :reader table-element-linkable-p)))

(defmethod calculate-graph-element-extent ((element table-element))
  (if (element-type-selectable-p (assoc (table-element-class element) *element-types*))
      (call-next-method)
      (let* ((area (graph-element-area element))
             (extent (cg:ncopy-box '#.(cg:make-box 0 0 0 0) area)))
        (pro:iincf (cg:box-right extent))
        (pro:iincf (cg:box-bottom extent))
        (cg:ncopy-box (slot-value element 'extent) extent))))

;;; When we are asked to redraw a part of the window we can
;;; use the cell sizes to work out which cells need to be
;;; redrawn, and then we can redraw only that area.
;;;
;;; We can convert from pixels to a box range by converting
;;; to and from the grid. The effect is that these will be
;;; used initially by the refresh code, but in practice by
;;; the hit testing code and other code too.

(defun floor-grid (value sizes &aux (index 0))
  (map nil #'(lambda (size)
               (when (< value size)
                 (return-from floor-grid index))
               (incf index)
               (decf value size))
       sizes)
  index)

(defun ceiling-grid (value sizes &aux (index 0))
  (when (< value 0) (return-from ceiling-grid 0))
  (map nil #'(lambda (size)
               (when (<= value size)
                 (return-from ceiling-grid (1+ index)))
               (incf index)
               (decf value size))
       sizes)
  index)

(defmethod nmake-grid-box-from-box ((table-element table-element) box)
  (let* ((row-height (grid-graph-element-row-height table-element))
         (class (slot-value table-element 'class))
         (template (slot-value table-element 'template))
         (column-widths (slot-value table-element 'column-widths))
         (break row-height)
         (title-height (pro:i+ (if template 0 *table-border-size*) row-height))
         (top (pro:i- (cg:box-top box) title-height))
         (bottom (pro:i- (cg:box-bottom box) title-height))
         (original-left (cg:box-left box)))
    (case class
      ((:instruction_card)
       (pro:iincf break row-height)))
    (setf (cg:box-top box)
            (pro:imax 0 
              (pro:i/ (if (pro:i> top break) (pro:i- top *table-border-size*) top)
                      row-height)))
    (setf (cg:box-bottom box)
            (pro:i/ (pro:i+ (pro:i1- row-height)
                            (if (pro:i> bottom break) (pro:i- bottom *table-border-size*) bottom))
                    row-height))
    (setf (cg:box-left box) (pro:imax 0 (floor-grid (cg:box-left box) column-widths)))
    (setf (cg:box-right box) (pro:imax (pro:i1+ (cg:box-left box)) (ceiling-grid (cg:box-right box) column-widths)))
    (let ((cells (grid-graph-element-cells table-element)))
      (setf (cg:box-bottom box) (pro:imin (cg:box-bottom box) (cg:position-y cells)))
      (setf (cg:box-right box) (pro:imin (cg:box-right box) (cg:position-x cells))))
    box))

;;; We need some special handling here. If the table is dynamic or special, we want 
;;; to extent the last column to the right of the element. 

(defmethod nmake-box-from-grid-box ((table-element table-element) box)
  (let* ((row-height (grid-graph-element-row-height table-element))
         (class (slot-value table-element 'class))
         (template (slot-value table-element 'template))
         (column-widths (slot-value table-element 'column-widths))
         (title-height (pro:i+ (if template 0 *table-border-size*) row-height))
         (break 0))
    (case class
      ((:instruction_card)
       (setf break 1)))
    (setf (cg:box-top box)
            (pro:i+ (pro:i* (cg:box-top box) row-height)
                    (if (pro:i> (cg:box-top box) break) *table-border-size* 0)
                    title-height))
    (setf (cg:box-bottom box)
            (pro:i+ (pro:i* (pro:i1+ (cg:box-bottom box)) row-height)
                    (if (pro:i> (cg:box-bottom box) break) *table-border-size* 0)
                    title-height))
    (setf (cg:box-left box) (loop for x from 0 below (cg:box-left box)
                                  for value in column-widths
                                  sum value))
    (setf (cg:box-right box) (loop for x from 0 below (pro:i1+ (cg:box-right box))
                                   for value in column-widths
                                   sum value))
    box))
   
;;; First things first. The height of the title depends on whether or not the table
;;; has a template, and whether or not we are dealing with a static or a dynamic 
;;; table. These can be marked in the Prolog format by something other than simple
;;; lists being in the table. In our system, we represent a set of rows and a table
;;; body separately. Switching from one to the other doesn't lose data. 

(defun set-grid-graphics-context (window element part &aux (class (and element (slot-value element 'class))))
  (cond ((eq class :trace_box)
         (cg:set-line-dashing window :solid)
         (cg:set-background-color window (if (graph-element-selected-p element)
                                             *header-colour*
                                             *white-colour*))
         (if (eq part :title)
             (cg:set-font window *trace-title-font*)
             (cg:set-font window *trace-body-font*)))
        ((eq part :resize)
         (cg:set-line-dashing window :solid)
         (cg:set-foreground-color window *black-colour*)
         (cg:set-background-color window *title-colour*))
        ((eq part :title)
         (cg:set-line-dashing window (element-type-line (assoc class *element-types*)))
         (cg:set-foreground-color window *black-colour*)
         (cg:set-background-color window (if (element-type-white-p (assoc class *element-types*)) *white-colour* *title-colour*))
         (cg:set-font window *table-title-font*))
        ((or (eq part :header) (and (cg:positionp part) (pro:izerop (cg:position-y part))))
         (cg:set-line-dashing window (element-type-line (assoc class *element-types*)))
         (cg:set-foreground-color window *black-colour*)
         (cg:set-background-color window (if (element-type-white-p (assoc class *element-types*)) *white-colour* *header-colour*))
         (cg:set-font window *table-body-font*))
        (t
         (cg:set-line-dashing window (if class (element-type-line (assoc class *element-types*)) :solid))
         (cg:set-foreground-color window *black-colour*)
         (cg:set-background-color window *white-colour*)
         (cg:set-font window *table-body-font*))))

#+Windows
(setf (symbol-function 'ensure-grid-graphics-context)
        (symbol-function 'set-grid-graphics-context))

#+Macintosh
(setf (symbol-function 'ensure-grid-graphics-context)
        (symbol-function 'set-grid-graphics-context))

;;; Now we get to the hard coded stuff. This is going to take a little
;;; work. 
     
(defmethod draw-grid-graph-element-cell ((element table-element) window cell)
  (let ((data (grid-graph-element-cell-data element cell))
        (area (graph-element-area element))
        (box (cg:nmake-box '#.(cg:make-box 0 0 0 0) (cg:position-x cell) (cg:position-y cell) (cg:position-x cell) (cg:position-y cell)))
        (origin '#.(cg:make-position 0 0))
        left top)
    (nmake-box-from-grid-box element box)
    (cg:nmake-position origin (cg:box-left area) (cg:box-top area))
    (cg:nbox-move box origin)
    (draw-table-element-part element window cell data box nil)))

;;; It would be nice if we could use the string width here, but we can't. This is
;;; because the EPSF generation device can't use it, because it doesn't have 
;;; access to PostScript font metrics. Instead, we'll just have to improvise, using
;;; the justifying code. Nasty though this is, it should work.

(defun draw-string (window string box centerp)
  (if centerp
      (let ((box (cg:ncopy-box '#.(cg:make-box 0 0 0 0) box)))
        (pro:iincf (cg:box-top box) *spreadsheet-border-y*)
        (pro:iincf (cg:box-left box) *spreadsheet-border-x*)
        (pro:idecf (cg:box-right box) *spreadsheet-border-x*)
        (cg:draw-string-in-box window string 0 (length string) box :center :top))
      (progn
        (cg:move-to-x-y window
          (pro:i+ (cg:box-left box) *spreadsheet-border-x*)
          (pro:i+ (cg:box-top box) *spreadsheet-border-y*))
        (write-string string window))))

(defun draw-table-element-part (element window part data box centerp)
  (let ((string (write-term-to-string data)))
    (ensure-grid-graphics-context window element part)
    ;; (print (list part box (and element (slot-value element 'class)) (cg:background-colour window '#.(cg:make-rgb ))))
    (cg:erase-contents-box window box)
    (cond ((slot-value element 'pendingp)
           (with-saved-foreground-colour (window)
             (cg:set-foreground-color window *pending-colour*)
             (draw-string window string box centerp)))
          ((and (cg:positionp part) (variablep data))
           (with-saved-foreground-colour (window)
             (cg:set-foreground-color window *variable-colour*)
             (draw-string window string box centerp)))
          (t
           (draw-string window string box centerp)))
    (cg:draw-box window box)))

;;; There are three parts to table elements: a title, the grid, and a body. We should
;;; really draw them in this order. The next method does the drawing for the grid 
;;; part. 

(defun draw-resize-button (window element box &key hflip vflip)
  (unless (graph-interactive-p window)
    (return-from draw-resize-button))
  (let* ((left (cg:box-left box))
         (top (cg:box-top box))
         (right (cg:box-right box))
         (bottom (cg:box-bottom box))
         (mid-x (pro:i/ (pro:i+ left right) 2))
         (mid-y (pro:i/ (pro:i+ top bottom) 2))
         (polygon '(#1=#.(cg:make-position 0 0)
                    #2=#.(cg:make-position 0 0)
                    #3=#.(cg:make-position 0 0)
                    #4=#.(cg:make-position 0 0)
                    #5=#.(cg:make-position 0 0)
                    #6=#.(cg:make-position 0 0)
                    #7=#.(cg:make-position 0 0)))
         (x1 (if hflip right left))
         (x2 (if hflip left right))
         (y1 (if vflip bottom top))
         (y2 (if vflip top bottom)))
     
    (cg:nmake-position #1# x1 y2)
    (cg:nmake-position #2# x2 y2)
    (cg:nmake-position #3# x2 y1)
    (cg:nmake-position #4# mid-x y1)
    (cg:nmake-position #5# mid-x mid-y)
    (cg:nmake-position #6# x1 mid-y)
    (cg:nmake-position #7# x1 y2)

    (ensure-grid-graphics-context window element :resize)
    (cg:erase-contents-polygon window polygon)
    (cg:draw-polygon window polygon)))

(defun draw-table-element-highlight (window area &aux (box '#.(cg:make-box 0 0 0 0)))
  (cg:ncopy-box box area)
  (pro:idecf (cg:box-left box) #.(floor (- *table-element-border-width* 1) 2))
  (pro:idecf (cg:box-top box) #.(floor (- *table-element-border-width* 1) 2))
  (pro:iincf (cg:box-right box) #.(floor *table-element-border-width* 2))
  (pro:iincf (cg:box-bottom box) #.(floor *table-element-border-width* 2))
  (nbox-inset box #.(- -1 *table-element-border-separation*) #.(- -1 *table-element-border-separation*))
  (cg:set-line-width window *table-element-border-width*)
  (cg:draw-box window box)
  (cg:set-line-width window *table-element-border-separation*)
  (cg:ncopy-box box area)
  (nbox-inset box (- *table-element-border-separation*) (- *table-element-border-separation*))
  (cg:set-foreground-color window cg:white)
  (cg:draw-box window box)
  (cg:set-line-width window 1))

(defun ntable-element-header-resize-box (box element area)
  (let ((cells (cg:ncopy-position '#.(cg:make-position 0 0) (grid-graph-element-cells element)))) 
    (cg:nposition+ cells '#.(cg:make-position -1 -1))
    (if (pro:i= (cg:position-x cells) -1)
        (cg:nmake-box box 0 0 1 1)
        (cg:nmake-box-from-corners box cells cells))
    (nmake-box-from-grid-box element box)
    (cg:nbox-move box (cg:nmake-position '#.(cg:make-position 0 0) (cg:box-left area) (cg:box-top area)))
    (if (pro:i= (cg:position-x cells) -1)
        (progn
          (setf (cg:box-right box) (pro:i+ (cg:box-left box) *resize-box-size*))
          (setf (cg:box-bottom box) (pro:i+ (cg:box-top box) *resize-box-size*))
          (values box t t))
        (progn
          (setf (cg:box-left box) (pro:i- (cg:box-right box) *resize-box-size*))
          (setf (cg:box-top box) (pro:i- (cg:box-bottom box) *resize-box-size*))
          (values box nil nil)))))

;;; Trace elements may add a little box to the title, a box that is used to show the status
;;; code when it has been set. This is only used occasionally, because we only need it for
;;; status elements in the left hand column. 
    
(defmethod draw-graph-element ((element table-element) window redraw-box highlightp
                               &aux (area (graph-element-area element))
                                    (box '#.(cg:make-box 0 0 0 0))
                                    (clip '#.(cg:make-box 0 0 0 0))
                                    (class (slot-value element 'class))
                                    (row-height (grid-graph-element-row-height element)))
  (cg:ncopy-box box area)
  (nbox-intersect box redraw-box)
   
  (cg:ncopy-box clip (graph-element-extent element))
  (nbox-intersect clip redraw-box)
  (pro:iincf (cg:box-right clip))
  (pro:iincf (cg:box-bottom clip))
  
;  (with-clipping-box (window clip)
    (ensure-grid-graphics-context window nil nil)
    (cg:erase-contents-box window box)
    (cg:ncopy-box box area)
  
    (let* ((template (slot-value element 'template))
           (status (slot-value element 'status))
           (title-height row-height))
      (setf (cg:box-bottom box) (pro:i+ (cg:box-top box) title-height))
      (if status
          (progn
            (setf (cg:box-left box) (pro:i- (cg:box-right box) *trace-element-status-width*))
            (when (cg:box-intersect-p box redraw-box)
              (draw-table-element-part element window :status status box t))
            (setf (cg:box-right box) (cg:box-left box))
            (setf (cg:box-left box) (cg:box-left area))))
     
      (when (cg:box-intersect-p box redraw-box)
        (draw-table-element-part element window :title (slot-value element 'title) box nil)))
          
    (cg:ncopy-box box area)
  
    (call-next-method)
     
    (ecase class
      ((:fact_card :question_box :trace_box))
      #||
      ((:command_card :special_box)
       (let ((box '#.(cg:make-box 0 0 0 0)))
         (cg:ncopy-box box area)
         (setf (cg:box-top box) (pro:i+ (cg:box-top area) row-height))
         (setf (cg:box-bottom box) (pro:i+ (cg:box-top box) *table-border-size*))
        
         ;; When we've drawn the box, draw the element's children recursively (or sort of)
         ;; but within a clipping region. 
        
         (cg:nmake-box box (cg:box-left area) (cg:box-bottom box)
           (cg:box-right area) (cg:box-bottom area))
         (pro:iincf (cg:box-left box))
         (pro:iincf (cg:box-top box))
        
         (redraw-clipped-graph window element box)))
      ||#
       
      ((:instruction_card)
     
       ;; Dynamic and special tables need a bit more work. First, we should try to 
       ;; draw the header background, and then any additional resize tabs. 
     
       (let ((box '#.(cg:make-box 0 0 0 0))
             (cells (cg:ncopy-position '#.(cg:make-position 0 0) (grid-graph-element-cells element))))
         (cg:ncopy-box box area)
         (if (pro:izerop (cg:position-x cells))
             (setf (cg:box-top box) (pro:i+ (cg:box-top area) (pro:i+ *empty-border-offset* *resize-box-size* row-height)))
             (setf (cg:box-top box) (pro:i+ (cg:box-top area) (pro:i* 3 row-height))))
         (setf (cg:box-bottom box) (pro:i+ (cg:box-top box) *table-border-size*))
         (cg:draw-box window box)
        
         ;; When we've drawn the box, draw the element's children recursively (or sort of)
         ;; but within a clipping region. 
        
         (cg:nmake-box box (cg:box-left area) (cg:box-bottom box)
           (cg:box-right area) (cg:box-bottom area))
         (pro:iincf (cg:box-left box))
         (pro:iincf (cg:box-top box))
        
         (redraw-clipped-graph window element box)
        
         ;; Also, draw the header resize box for dynamic and special tables. Do this by
         ;; getting the last cell and putting it in there. 
        
         (multiple-value-bind (box hflip vflip) (ntable-element-header-resize-box box element area)
           (when (and (cg:box-intersect-p box redraw-box)
                      (not (typep window 'cg:printer)))
             (draw-resize-button window element box :hflip hflip :vflip vflip))))))

    (when (and highlightp 
               (element-type-selectable-p (assoc class *element-types*))
               (typep window 'cg:window))
      (draw-table-element-highlight window area))
   
    (cg:ncopy-box box area)
    (setf (cg:box-left box) (pro:i- (cg:box-right box) *resize-box-size*))
    (setf (cg:box-top box) (pro:i- (cg:box-bottom box) *resize-box-size*))
    (when (and (cg:box-intersect-p box redraw-box)
               (not (typep window 'cg:printer))
               (element-type-resizeable-p (assoc class *element-types*)))
      (draw-resize-button window element box :hflip nil :vflip nil))
    (ensure-grid-graphics-context window nil nil))
;   )
   
;;; We should really make the element here first, and then install the area at the
;;; end. In fact, we really want a dispatching function to calculate the area, but
;;; this can wait. 
;;;
;;; We need some extra stuff here. Dynamic tables (which always have a template)
;;; will only ever have one row. Question tables (which may or may not have a 
;;; template, will have one other row). Static tables may have any number of rows
;;; after the template. We should restrict the element area accordingly. 
;;; The area of a dynamic table (and a special table) is calculated through the 
;;; body. 

(defun calculate-table-element-area (element)
  (let ((area '#.(cg:make-box 0 0 0 0))
        (class (table-element-class element))
        (offset '#.(cg:make-position 0 0))
        (old-area (graph-element-area element)))
    (case class
      ((:fact_card :question_box :trace_box)
       (let* ((cells (grid-graph-element-cells element))
              (cell-box (cg:nmake-box '#.(cg:make-box 0 0 0 0) 0 0
                          (pro:i1- (cg:position-x cells)) (pro:i1- (cg:position-y cells)))))
         (nmake-box-from-grid-box element cell-box)
         (setf (cg:box-left cell-box) 0)
         (setf (cg:box-top cell-box) 0)
         (cg:ncopy-box area cell-box)))
      #||
      ((:command_card :special_box)
       (let ((body-size (table-element-body-size element))
             (header (pro:i+ (grid-graph-element-row-height element) *table-border-size*)))
         (cg:nmake-box area 0 0 (cg:position-x body-size) (pro:i+ header (cg:position-y body-size)))))
      ||#
      ((:instruction_card)
       (let* ((cells (grid-graph-element-cells element))
              (body-size (table-element-body-size element))
              (cell-box (cg:nmake-box '#.(cg:make-box 0 0 0 0) 0 0
                          (pro:i1- (cg:position-x cells)) (pro:i1- (cg:position-y cells)))))
         (nmake-box-from-grid-box element cell-box)
         (setf (cg:box-left cell-box) 0)
         (setf (cg:box-top cell-box) 0)
         (setf (cg:box-right cell-box) (pro:imax (cg:box-right cell-box)
                                         (cg:position-x body-size)))
         (setf (cg:box-bottom cell-box) (pro:i+ 2 (cg:box-bottom cell-box)
                                          (cg:position-y body-size)))
         (cg:ncopy-box area cell-box))))
    (cg:nmake-position offset (cg:box-left old-area) (cg:box-top old-area))
    (cg:nbox-move area offset)
    area))

(defmethod graph-element-interior ((element table-element))
  (let ((area (cg:ncopy-box '#.(cg:make-box 0 0 0 0) (graph-element-area element)))
        (class (slot-value element 'class)))
    (pro:iincf (cg:box-left area))
    (pro:iincf (cg:box-top area))
    (ecase class
      ((:fact_card :question_box :trace_box))
      #||
      ((:command_card :special_box)
       (pro:iincf (cg:box-top area) (pro:i+ *table-border-size* (grid-graph-element-row-height element))))
      ||#
      ((:instruction_card)
       (let* ((cells (cg:ncopy-position '#.(cg:make-position 0 0) (grid-graph-element-cells element)))
              (row-height (grid-graph-element-row-height element))
              (title-height (pro:i+ (if (pro:iplusp (cg:position-x cells))
                                        (pro:i+ row-height row-height)
                                        (pro:i+ *empty-border-offset* *resize-box-size*))
                                    *table-border-size*
                                    row-height)))
         (pro:iincf (cg:box-top area) title-height))))
    area))

(defmethod object-named-p ((element table-element) name)
  (string-equal name (table-element-title element)))
