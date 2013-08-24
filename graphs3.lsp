;;; -*- Mode: Lisp; Package: HANK -*-
;;;
;;; Author: Stuart Watt
;;;         Open University
;;;
;;; Design: Paul Mulholland
;;;         Stuart Watt
;;;         Open University

(in-package "HANK")

;;; Code for tracking and handling the linking process. This is used when we
;;; drag with the pencil tool. The idea is that we should do all the manhattan
;;; interpolation code, so that when we drag a link it looks nice on the 
;;; screen. When we've done this, and managed to get links saved, we have most
;;; of what we need for a first version of the Hank system. 
;;;
;;; Now we can define the new graph element, and add it to the drawing. These graph
;;; elements contain a segment, and perhaps a few other little things. We then 
;;; add them to the graph and everything should be hunky dory. Finally, it would
;;; be nice if we could create links from the keyboard, but that can wait. 
;;;
;;; One end of the link should be designated an arrow point, to show where we're
;;; drawing to. Actually, this is never a problem, because it is, genuinely, the
;;; end of the link. We can, therefore, always get the direction output of the
;;; previous position. 

(defclass drag-link (drag-window-mixin drag)
  ((segment
     :initarg :segment
     :reader drag-link-segment)
   (from-element
     :initarg :from-element
     :reader drag-link-from-element)
   (to-element
     :initform nil
     :accessor drag-link-to-element)
   (from-position
     :initarg :from-position
     :reader drag-link-from-position)
   (from-point
     :initarg :from-point
     :reader drag-link-from-point)
   (to-position
     :initform (cg:make-position 0 0)
     :initarg :to-position
     :reader drag-link-to-position)
   (to-point 
     :initarg :to-point
     :accessor drag-link-to-point)))

(defmethod graph-drag-link-element ((window graph-window) element buttons position)
  (loop for selected-element in (graph-selected-elements window)
        unless (eq element selected-element)
        do (setf (graph-element-selected-p selected-element) nil))
  (let* ((point-position '#.(cg:make-position 0 0))
         (area (graph-element-area element))
         (point (attachment-point area position))
         (segment (create-segment)))
    (nattachment-point-position area point-position point)
    (nmake-segment segment point-position point position nil)
    (let ((drag (make-drag 'drag-link window buttons (cg:copy-position position)
                  :from-element (first (graph-selected-elements window))
                  :from-position point-position
                  :from-point point
                  :segment segment)))
      (drag-track drag))))

(defclass drag-edit-link (drag-link)
  ((element
     :initarg :element
     :reader drag-edit-link-element)
   (reversep
     :initarg :reversep
     :reader drag-edit-link-reverse-p)))

(defmethod graph-drag-link-edit-element ((window graph-window) element part buttons position)
  (let* ((point-position '#.(cg:make-position 0 0))
         (connection (link-element-connection element))
         (from-element (contact-element (connection-from connection)))
         (to-element (contact-element (connection-to connection)))
         (drag-from-element (if (eq part :start) to-element from-element))
         (drag-to-element (if (eq part :start) from-element to-element))
         (from-point (contact-point (connection-from connection)))
         (to-point (contact-point (connection-to connection)))
         (drag-from-position '#.(cg:make-position 0 0))
         (drag-to-position '#.(cg:make-position 0 0))
         (drag-from-point (if (eq part :start) to-point from-point))
         (drag-to-point (if (eq part :start) from-point to-point))
         (segment (create-segment)))
    (nattachment-point-position (graph-element-area drag-from-element) drag-from-position drag-from-point)
    (nattachment-point-position (graph-element-area drag-to-element) drag-to-position drag-to-point)
    (let ((drag (make-drag 'drag-edit-link window buttons (cg:copy-position position)
                  :element element
                  :reversep (eq part :start)
                  :from-element drag-from-element
                  :from-position drag-from-position
                  :from-point drag-from-point
                  :to-position drag-to-position
                  :to-point drag-to-point
                  :segment segment)))
      (drag-track drag))))

;;; Segments are drawn as a polyline, a list of positions. Cunningly, we have an index
;;; which tells us where to start drawing from. This allows us to change the number of
;;; links in the segment dynamically. 

(defun drag-track-draw-tracking (tracking window drawp)
  (with-saved-paint-operation (window)
    (cg:set-paint-operation window equal)
    (cg:set-line-dashing window :dot)
    (unwind-protect (progn
                      (cg:draw-polyline window (nthcdr (first tracking) (rest (rest tracking))))
                      (when (second tracking)
                        (let ((position (nth 7 tracking)))
                          (cg:fill-circle window position 5))))
      (cg:set-line-dashing window :solid))))

(defmethod drag-hide-tracking ((drag drag-link))
  (let ((tracking (drag-old-tracking drag)))
    (drag-track-draw-tracking tracking (slot-value drag 'source-window) nil)))

(defmethod drag-show-tracking ((drag drag-link))
  (let ((tracking (drag-old-tracking drag)))
    (drag-track-draw-tracking tracking (slot-value drag 'source-window) t)))

(defmethod drag-make-tracking ((drag drag-link))
  (let ((tracking (drag-link-segment drag)))
    (list* (first tracking)
           (second tracking)
           (loop for position in (rest (rest tracking))
                 collect (cg:copy-position position)))))

(defmethod drag-copy-tracking ((drag drag-link) new-tracking old-tracking)
  (setf (first new-tracking) (first old-tracking))
  (setf (second new-tracking) (second old-tracking))
  (loop for new-position in (rest (rest new-tracking))
        for old-position in (rest (rest old-tracking))
        do (cg:ncopy-position new-position old-position))
  new-tracking)

(defmethod drag-move-tracking ((drag drag-link) new-position old-position)
  (error "Not implemented"))

;;; Getting the approved tracking is the tricky bit. At this point, we should
;;; look for an element under the position. If we find one, and we're within 
;;; reach of an attachment point, use the attachment point instead of the 
;;; mouse position. 

(defmethod drag-get-approved-tracking ((drag drag-link) new-tracking new-position)
  (let* ((mouse-position (cg:ncopy-position '#.(cg:make-position 0 0) new-position))
         (mouse-point nil)
         (window (slot-value drag 'source-window))
         (element (get-element-at-position window mouse-position
                    :test #'(lambda (element)
                              (not (typep element 'link-element))))))
    (when (and element 
               (typep element 'node-element)
               (eq (graph-element-parent element)
                   (graph-element-parent (drag-link-from-element drag))))
      (let* ((area (graph-element-area element))
             (box (cg:ncopy-box '#.(cg:make-box 0 0 0 0) area)))
        (nbox-inset box *attachment-point-scope* *attachment-point-scope*)
        (unless (cg:inside-box-p mouse-position box)
          (setf mouse-point (attachment-point area mouse-position))
          (nattachment-point-position area mouse-position mouse-point))))
    (nmake-segment new-tracking
      (drag-link-from-position drag) (drag-link-from-point drag)
      mouse-position mouse-point)
    (setf (second new-tracking) (if mouse-point t nil))
    (cg:ncopy-position (drag-link-to-position drag) mouse-position)
    (setf (drag-link-to-element drag) (when mouse-point element))
    (setf (drag-link-to-point drag) mouse-point)
    new-tracking))

(defmethod call-drag-event ((drag drag-link) window event buttons data time)
  (declare (special cg::character))
  (let ((new-position '#.(cg:make-position 0 0))
        (old-position (cg:ncopy-position '#.(cg:make-position 0 0) (drag-current-position drag))))
    (if (cg:positionp data)
        (progn
          (cg:ncopy-position new-position data)
          (when (drag-scroll-if-required drag window data)
            (cg:ncursor-position window new-position)))
        (cg:ncursor-position window new-position))
    (when (or (pro:i= event cg:virtual-key-down)
              (pro:i= event cg:virtual-key-up)
              (pro:i= event cg::character)
              (pro:i= event cg:timer-event)
              (pro:i= event cg:mouse-moved)
              (cg:mouse-event-p event))
      
      ;; We should be a bit smarter than this. Instead of simply using positions, use
      ;; an adjusted "approved" box. This will be the best box approved by the new
      ;; position. Only if the box is different from the old box do we redraw anything. 
      ;; Again, this should use the "approved" box. The old box should always be 
      ;; approved. We can set this up when we start the drag. This will mean we're not
      ;; approving all the time. 
       
      (let ((old-tracking (drag-old-tracking drag))
            (new-tracking (drag-new-tracking drag)))
        (drag-get-approved-tracking drag new-tracking new-position)
        (unless (drag-tracking-equal drag old-tracking new-tracking)
          (drag-hide-tracking drag)
          (drag-copy-tracking drag old-tracking new-tracking)
          (drag-show-tracking drag)
          (cg:ncopy-position (drag-current-position drag) new-position)))
      (drag-event drag window event buttons new-position data))))

(defmethod drag-tracking-equal ((drag drag-link) tracking1 tracking2 &aux length)
  (and (= (setf length (first tracking1)) (first tracking2))
       (eq (second tracking1) (second tracking2))
       (loop for position1 in (nthcdr length (rest (rest tracking1)))
             for position2 in (nthcdr length (rest (rest tracking2)))
             if (not (cg:position= position1 position2))
               do (return nil)
             end
             finally do (return t))))

(defun draw-basic-line-end (window position angle &key (spread 35) (length *arrow-length*))
  (let* ((reversed-angle (+ angle 180))
         (half-spread (/ spread 2))
         (offset (cg:nmake-position '#.(cg:make-position 0 0) length 0))
         (start (cg:ncopy-position '#1=#.(cg:make-position 0 0) position))
         (side1 (cg:ncopy-position '#2=#.(cg:make-position 0 0) start))
         (side2 (cg:ncopy-position '#3=#.(cg:make-position 0 0) start))
         (end (cg:ncopy-position '#4=#.(cg:make-position 0 0) start)))
    (cg:nposition-rotate offset reversed-angle)
    (cg:nposition+ end offset)
    (cg:nmake-position offset length 0)
    (cg:nposition-rotate offset (+ reversed-angle half-spread))
    (cg:nposition+ side1 offset)
    (cg:nmake-position offset length 0)
    (cg:nposition-rotate offset (- reversed-angle half-spread))
    (cg:nposition+ side2 offset)
    (cg:fill-polygon window '(#1# #2# #4# #3#))
    end))

(defun calculate-link-element-area (element)
  (let ((minx most-positive-fixnum)
        (miny most-positive-fixnum)
        (maxx most-negative-fixnum)
        (maxy most-negative-fixnum)
        (label-box (nlink-element-label-box element '#.(cg:make-box 0 0 0 0)))
        (result '#.(cg:make-box 0 0 0 0)))
    (loop for position in (link-element-segment element)
          do (setf minx (pro:imin minx (cg:position-x position)))
             (setf miny (pro:imin miny (cg:position-y position)))
             (setf maxx (pro:imax maxx (cg:position-x position)))
             (setf maxy (pro:imax maxy (cg:position-y position))))
    (cg:nmake-box result minx miny maxx maxy)
    (when label-box  
      (nbox-union result label-box))
    result))

(defmethod graph-drag-handler ((window graph-window) (element link-element) part buttons position)
  (case part
    ((:start :end)
     (graph-drag-link-edit-element window element part buttons position))))

;;; To create a link, we need two refer to two elements, and to use two connection
;;; points. The selected element should always be the first, so all we need in a
;;; script command is a reference to a second element, and two connection points. 
;;; After the command, the link is selected. 

(defun make-or-replace-link (window label script-label connection replace)
  (let* ((from-element (contact-element (connection-from connection)))
         (to-element (contact-element (connection-to connection)))
         (from-area (graph-element-area from-element))
         (to-area (graph-element-area to-element))
         (from-point (contact-point (connection-from connection)))
         (to-point (contact-point (connection-to connection)))
         (from-position (nattachment-point-position from-area '#.(cg:make-position 0 0) from-point))
         (to-position (nattachment-point-position to-area '#.(cg:make-position 0 0) to-point))
         (parent (graph-element-parent from-element))
         (segment (create-segment)))
    (nmake-segment segment from-position from-point to-position to-point)
    (let* ((graph-element 
             (make-instance 'link-element
               :window window
               :connection connection
               :segment (nthcdr (first segment) (rest (rest segment)))
               :parent parent))
           (command (make-command
                      :type 'update-link
                      :window window
                      :do-data (list (list graph-element) (list parent (list graph-element) replace))
                      :undo-data (list (graph-selected-elements window) (list parent replace (list graph-element)))
                      :label label
                      :do-function #'object-clipboard-command-do-function
                      :modify-window (graph-editor-window window)
                      :undo-function #'object-clipboard-command-undo-function)))
       (setf (graph-element-area graph-element t)
               (cg:copy-box (calculate-link-element-area graph-element)))
       (record-command (application-window-main-window (graph-editor-window window))
         (make-compound script-label
           (list (list (generate-objects-resolver (list from-element) window) from-point)
                 (list (generate-objects-resolver (list to-element) window) to-point))))
       (execute command))))

(comtab:defmenu-command :link ((window graph-window))
  (let ((command *script-command*))
    (when command
      (let ((values (compound-arguments command)))
        (make-or-replace-link window "Link" "Link"
          (make-connection (make-contact (resolve-path (first (first values)) window) (second (first values)))
                           (make-contact (resolve-path (first (second values)) window) (second (second values))))
          ())))))
        
(defmethod drag-drop-window ((drag drag-link) window buttons position)
  (make-or-replace-link window "Link" "Link" 
    (make-connection (make-contact (drag-link-from-element drag) (drag-link-from-point drag))
                     (make-contact (drag-link-to-element drag) (or (drag-link-to-point drag)
                                                                   (return-from drag-drop-window nil))))
    ()))

;;; Editing a link is similar to creating a link, except that we start with the link being
;;; selected and, in principle, any of the four main parts to the connection can be changed. 

(defmethod drag-drop-window ((drag drag-edit-link) window buttons position)
  (make-or-replace-link window "Edit Link" "Edit Link"
    (let ((connection (make-connection 
                        (make-contact (drag-link-from-element drag) (drag-link-from-point drag))
                        (make-contact (drag-link-to-element drag) (or (drag-link-to-point drag)
                                                                      (return-from drag-drop-window nil))))))
      (if (drag-edit-link-reverse-p drag)
          (nreverse connection)
          connection))
    (list (drag-edit-link-element drag))))

(defmethod graph-element-part ((element link-element) point)
  (let* ((segment (link-element-segment element))
         (length (length segment)))
    (when (point-on-point-p point (first segment))
      (return-from graph-element-part :start))
    (when (point-on-point-p point (nth (pro:i1- length) segment))
      (return-from graph-element-part :end))
    (let ((label-box (nlink-element-label-box element '#.(cg:make-box 0 0 0 0))))
      (when (and label-box (cg:inside-box-p point label-box))
        (return-from graph-element-part :label)))
    (loop for segment = segment then (rest segment)
          if (null (rest segment))
            do (return nil)
          end
          if (point-on-line-p point (first segment) (second segment))
            do (return t)
          end)))

;;; Where's the middle of the link, in a way that we can draw a text label for
;;; it in a sensible form. This should take a link and a label and return a 
;;; box which places the label sensibly. This box can then be passed to
;;; draw-string-in-box to draw the label. 
;;;
;;; All segments are either odd or even in length, and if odd, there is a corner 
;;; in the middle, otherwise, there is a link. 
;;;
;;; It would be nice if we didn't have to calculate the box, but we do, because
;;; the box may be used in area calculations. For example, the elements area
;;; must include the entire label box, and a click on the label box should
;;; count as a click on the link. However, we can cache a lot of stuff regarding
;;; the label width and height. 

(defun link-element-label-size (element)
  (with-slots (label-size) element
    (or label-size
        (setf label-size (let* ((window (graph-element-window element))
                                (label (link-element-label element))
                                (old-font (cg:font-handle window))
                                (position (cg:make-position 0 0)))
                           (unwind-protect (progn
                                             (cg:set-font window *link-label-font*)
                                             (setf (cg:position-y position) (cg:line-height window))
                                             (setf (cg:position-x position)
                                                     (cg:stream-string-width window label)))
                             (cg:set-font window old-font))
                           position)))))

(defun nlink-element-label-box (element box)
  (let* ((segment (link-element-segment element))
         (length (length segment))
         (label (link-element-label element))
         (label-size (link-element-label-size element))
         (position nil))
    (if (pro:ioddp length)
        (setf position (nth (pro:i/ length 2) segment))
        (let* ((offset (pro:i1- (pro:i/ length 2)))
               (start (nth offset segment))
               (end (nth (pro:i1+ offset) segment)))
          (setf position (cg:nmake-position '#.(cg:make-position 0 0)
                           (pro:i/ (pro:i+ (cg:position-x start) (cg:position-x end)) 2)
                           (pro:i/ (pro:i+ (cg:position-y start) (cg:position-y end)) 2)))))
    (nbox-justify (cg:nmake-box-from-corners box '#.(cg:make-position 0 0) label-size)
      (cg:nmake-box-from-corners '#.(cg:make-box 0 0 0 0) position position)
      :center :center)
    box))

(defmethod draw-graph-element ((element link-element) window box highlightp)
  (when highlightp (cg:set-line-width window 2))
;;  (print (cg:nclipping-box window '#.(cg:make-box 0 0 0 0)))
;;  (print (graph-element-area element))
  (let* ((segment (link-element-segment element))
         (length (length segment))
         (end (nth (pro:i1- length) segment))
         (previous (nth (pro:i- length 2) segment))
         (label (link-element-label element))
         (label-box (nlink-element-label-box element '#.(cg:make-box 0 0 0 0))))
    (cg:draw-polyline window segment)
    (when label
      (cg:set-font window *link-label-font*)
      (cg:draw-string-in-box window label 0 (length label) label-box :center :center))
    (when highlightp 
      (cg:fill-circle window (nth 0 segment) 3)
      (cg:fill-circle window end 3))
    (draw-basic-line-end window end (pro:radians-to-degrees
                                      (atan (pro:i- (cg:position-y end) (cg:position-y previous))
                                            (pro:i- (cg:position-x end) (cg:position-x previous))))))
  (when highlightp (cg:set-line-width window 1)))

(defun complete-link-label-editor-function (window editor-descriptor status)
  (let* ((element (getf (rest editor-descriptor) 'element))
         (graph-window (cg:stream-location window))
         (old-value (link-element-label element))
         (new-value (subseq (teb) 0 (get-window-text window))))
    (unless (string= (write-term-to-string old-value) new-value)
      (let ((command (make-command
                       :type 'change-label
                       :window graph-window
                       :modify-window (graph-editor-window graph-window)
                       :label "Label"
                       :do-data (list (list element new-value))
                       :undo-data (list (list element old-value))
                       :do-function #'link-label-command-do-function
                       :undo-function #'link-label-command-undo-function)))
        (execute command)
        (record-command (application-window-main-window (graph-editor-window graph-window))
          (make-compound "Label Link" (list new-value)))))))

;;; If the label is empty, before we open the text editor, we should make sure
;;; that the box is viable. 

(defmethod selected-element ((element link-element) (part (eql :label)) window)
  (unless *preference-free-link-names-p*
    (return-from selected-element (call-next-method)))
  (complete-element-editor window :new-table)
  (cg:set-font window *link-label-font*)
  (let* ((box (nlink-element-label-box element '#.(cg:make-box 0 0 0 0)))
         (width (cg:box-width box)))
    (when (pro:i< width *link-label-minimum-width*)
      (nbox-inset box (pro:i/ (pro:i- width *link-label-minimum-width*) 2) 0))
    (install-element-editor element window box '(:justification :center)
      (link-element-label element)
      `(element ,element function complete-link-label-editor-function))))

(defun link-label-command-do-function (command)
  (link-label-command-apply-function command (command-do-data command)))
(defun link-label-command-undo-function (command)
  (link-label-command-apply-function command (command-undo-data command)))

(defgeneric (setf link-element-label) (value element &optional recursivep))

(defun link-label-command-apply-function (command data)
  (loop for entry in data
        do (destructuring-bind (element value) entry
             (setf (link-element-label element) value))))

(defmethod (setf link-element-label) (value (element link-element) &optional recursivep)
  (when recursivep
    (setf (slot-value element 'label) value)
    (setf (slot-value element 'label-size) nil)
    (return-from link-element-label))
  (setf (link-element-label element t) value)
  (let ((box (cg:ncopy-box '#.(cg:make-box 0 0 0 0) (graph-element-area element)))
        (window (graph-element-window element)))
    (when window
      (cg:erase-contents-box window box)
      (redraw-clipped-graph window window box))))

(defmethod :open-selection ((window graph-window))
  (let ((selection (graph-selected-elements window)))
    (when (= (length selection) 1)
      (let ((element (first selection)))
        (selected-element element :enter window)))))

(defmethod selected-element ((element link-element) (part (eql :enter)) window)
  (selected-element element :label window))

