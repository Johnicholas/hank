;;; -*- Mode: Lisp; Package: HANK -*-
;;;
;;; Author: Stuart Watt
;;;         The Open University
;;;
;;; Design: Paul Mulholland
;;;         Stuart Watt
;;;         The Open University

(in-package "HANK")

;;; Printing and exporting are rather similar. The main difference is that for printing
;;; we do not need to write our own device driver for Common Graphics. We can simply use
;;; the one that's built in, and hope. However, we should be a bit careful about 
;;; printing, and we should record various bits of printer information to make this
;;; reasonable. 
;;;
;;; As usual, this is about my nth attempt to get printing to go in a sensible manner. 
;;; This time I've got the source code to help, so here's hoping. Even so, much of the
;;; printing interface seems simply to be unworkable. The nice simple interface to 
;;; printing offered by Microsoft does things behind the back of all the other 
;;; printing things, so we never get to hear about them. For now, don't worry about page
;;; set up or anything like that. Just work with the printing dialog. 
;;;
;;; Nasty stuff means that there are a few slight differences between the CG window
;;; and printer imaging models. I have yet to work out what most of these are, but
;;; I believe they are to do with the handling of lines and polygons, in the main. These
;;; should really be fixed in the CG printer interface, but I'm not hoping. On the
;;; other hand, they may be just the way things work, given that screens are lower
;;; resolution than printers. 

(defgeneric print-elements (window graph elements))

(defclass printer-graph (graph cg:printer)
  ())

(comtab:defmenu-command :print-selection ((window editor-window))
  (multiple-value-bind (graph-window selection) (:selection window)
    (print-elements window graph-window selection)))

(comtab:defmenu-command :print ((window workspace-window))
  (let ((graph-window (cg:get-stream-prop window 'output-window)))
    (print-elements window graph-window (graph-elements graph-window))))

(comtab:defmenu-command :print ((window editor-window))
  (let ((graph-window (cg:selected-window window)))
    (print-elements window graph-window (graph-elements graph-window))))

(defconstant *printer-resolution* 200)

;;; Before we actually print, we need to rebind the colours -- all the colours --
;;; so that printing looks nice. 

(defmacro with-rgb-colours (&body body)
  `(let ,(loop for element in *colour-source*
               collect (list (first element)
                             (cg:make-rgb :red (colour-red (second element))
                                          :green (colour-green (second element))
                                          :blue (colour-blue (second element)))))
     ,@body))

;;; The main difference between printing and exporting is that printing needs to support
;;; page spanning.  This makes things a bit harder to do than should otherwise be the
;;; case.  Anyway, now we can actually see page spanning happen, let's get on with it.  

(defmethod print-elements ((window application-window) graph elements)
  (let* ((terms (loop for (element graph-element) in (get-file-elements elements)
                      collect element))
         (pages (graph-pages graph))
         (output (cg:open-stream 'printer-graph 'cg:null-location :output
                   :paper-size :a4
                   :start 1 :end (* (cg:position-x pages) (cg:position-y pages))
                   :min-start 1 :max-end (* (cg:position-x pages) (cg:position-y pages))
                   :disable-selection t)))
    (unless output
      (return-from print-elements))
    (unwind-protect (let ((start-page (or (cg:start-page output) 1))
                          (end-page (or (cg:end-page output) (* (cg:position-x pages) (cg:position-y pages)))))
                      (cg:process-pending-events)
                      (setf (cg:stream-units-per-inch output) *printer-resolution*)
                      (with-resolution-variables
                        (initialise-layout-parameters :resolution *printer-resolution*)
                       
                        ;; Now we do the hard stuff.  Turn the forms into elements and
                        ;; pass them to the printer.  This is where pagination needs to
                        ;; come in.  We should look at the top level elements, and make
                        ;; sure they're in the right place before actually adding them 
                        ;; to the window.  
                       
                        (let ((horizontal-margin (- (* (cg:paper-width output) *internal-resolution*) a4-visible-width))
                              (vertical-margin (- (* (cg:paper-length output) *internal-resolution*) a4-visible-height)))
                          (cg:set-left-margin output (resolve (ceiling horizontal-margin 2)) t)
                          (cg:set-right-margin output (resolve (floor horizontal-margin 2)) t)
                          (cg:set-top-margin output (resolve (ceiling vertical-margin 2)) t)
                          (cg:set-bottom-margin output (resolve (floor vertical-margin 2)) t)
                          (cg:set-stream-origin output
                            (cg:make-position (resolve (ceiling horizontal-margin 2)) (resolve (ceiling vertical-margin 2)))))
                       
                        (loop with firstp = t
                              for page from start-page to end-page
                              do (unless firstp
                                   (acl:new-page output))
                                 (setf firstp nil)
                                 (let ((elements (get-graph-elements-from-forms terms output))
                                       (page-x (pro:imod (pro:i1- page) (cg:position-x pages)))
                                       (page-y (pro:i/ (pro:i1- page) (cg:position-x pages))))
                                   (loop for element in elements
                                         do (set-window element output))
                                   (setf (graph-elements output) elements)
                                   (with-rgb-colours
                                     (cg:set-background-color output *header-colour*)
                                     (cg:erase-contents-box output '#.(cg:make-box -2767 -2767 2767 2767))
                                     (cg:set-background-color output cg:white)
                       
                                     (print-graph window graph output elements))
                                   (cg:window-message (application-window-main-window window)
                                     "Printed page ~D" page)))))
      (close output))))

(defmethod print-elements ((window editor-window) graph elements)
  (complete-element-editor (editor-window-editor window) :end)
  (unless elements
    (return-from print-elements nil))
  (call-next-method))

;;; The heart of the printing system.  It redraws the graph.  However, we should do this
;;; on a per-page basis, and we also need to make sure we're handling offsetting and 
;;; all that sort of thing properly.  Usually we gloss this stuff, but we actually can't
;;; afford to. 

(defmethod print-graph ((window application-window) graph output elements)
  (redraw-graph output output
    '#.(cg:make-box most-negative-fixnum most-negative-fixnum 
                    most-positive-fixnum most-positive-fixnum)))

(defmethod print-graph ((window workspace-window) graph output elements)
  (when elements
    (let ((box (cg:copy-box (graph-element-area (first elements)))))
      (loop for element in (rest elements)
            do (nbox-union box (graph-element-area element)))
      (setf (cg:box-left box) 0)
      (redraw-workspace-grid output box)
      (call-next-method))))

;;; Nasty thought it looks, this works around most of the problems and bugs in Allegro's printing
;;; code.  With this, we can get the actual margins and everything right.  We just have to be 
;;; careful with the page size, because that could, in theory, be changed and would then differ 
;;; from the page size set in the rest of the system.  All this is because doing a Page Setup in 
;;; Windows is hideously complicated. 
;;;
;;; Next, we need to link this to the rest of the printing system and set it all working on
;;; multiple pages.  But before that, I need a break!!

(defun test-print ()
  (let ((cg:*default-printer-left-margin* 1000)
        (cg:*default-printer-top-margin* 1000)
        (cg:*default-printer-right-margin* 1000)
        (cg:*default-printer-bottom-margin* 1000)
        (printer-resolution nil)
        (requested-resolution 200))
  (with-open-stream (print (cg:open-stream 'printer-graph 'cg:null-location :output
                            :paper-size :a4))
    (setf printer-resolution (cg:stream-units-per-inch print))
    (setf (cg:stream-units-per-inch print) requested-resolution)
    (let* ((units (cg:stream-units-per-inch print))
           (offset (cg:printer-physical-offset print))
           (dx (/ (* units (cg:position-x offset)) printer-resolution))
           (dy (/ (* units (cg:position-y offset)) printer-resolution))
           (width (floor (* units (cg:paper-width print))))
           (height (floor (* units (cg:paper-length print))))
           (left (floor (- (/ (* cg:*default-printer-left-margin* units) 1000) dx)))
           (top (floor (- (/ (* cg:*default-printer-top-margin* units) 1000) dy)))
           (right (floor (+ (/ (* cg:*default-printer-right-margin* units) 1000) dx)))
           (bottom (floor (+ (/ (* cg:*default-printer-bottom-margin* units) 1000) dy))))
      (cg:set-clipping-box print
        (print (cg:make-box left top (- width right) (- height bottom))))
      (cg:set-stream-origin print (cg:make-position left top))
    
      (loop for x from (- (* units (ceiling 1000 units))) to 2000 by units
            do (cg:move-to print (cg:make-position (- (* units (ceiling 1000 units))) x))
               (cg:draw-to print (cg:make-position 2000 x))
               (cg:move-to print (cg:make-position x (- (* units (ceiling 1000 units)))))
               (cg:draw-to print (cg:make-position x 2000)))
      (cg:draw-circle print (cg:make-position 0 0) 100)
      (cg:draw-circle print (cg:make-position 0 0) 200)
      (cg:draw-circle print (cg:make-position 0 0) 300)
      (cg:draw-circle print (cg:make-position 0 0) 400)
      (cg:draw-circle print (cg:make-position 0 0) 500)))))

#||
(test-print)
||#