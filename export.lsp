;;; -*- Mode: Lisp; Package: HANK -*-
;;;
;;; Author: Stuart Watt
;;;         The Open University
;;;
;;; Design: Paul Mulholland
;;;         Stuart Watt
;;;         The Open University

(in-package "HANK")

;;; Some export functions, which we're going to use for the rest of the
;;; system. These export functions turn the graphs in a window, or just a
;;; selected part of these graphs, into other forms. These forms generally
;;; allow us to save part of the display in a useful format. 

(defconstant export-types
  '((:eps ("Encapsulated PostScript" . "*.eps"))))

(defmethod export-pathname ((window editor-window))
  (editor-window-pathname window))

(defun commence-export-elements (window elements command)
  (let* ((old-pathname (export-pathname window)))
    (multiple-value-bind (pathname format)
        (when *script-command*
          (let ((file (string-getf (compound-arguments *script-command*) "File"))
                (format (string-getf (compound-arguments *script-command*) "Format")))
            (when file
              (setf file (pathname file)))
            (values file (or (position format export-types :key #'(lambda (element)
                                                                    (first (second element)))
                               :test #'string-equal)
                             0))))
      (unless pathname
        (multiple-value-setq (pathname format)
          (cg:ask-user-for-new-pathname "Export Selection As"
            :host (when old-pathname 
                    (directory-namestring old-pathname))
            :allowed-types (loop for type in export-types
                                 collect (second type))
            :initial-name (namestring
                            (make-pathname :type "eps" :defaults (pathname (cg:stream-title window)))))))
      (when pathname
        (record-command (application-window-main-window window) 
          (make-compound command (list "File" (namestring pathname)
                                       "Format" (first (second (elt export-types format))))))
        (export-elements window (first (elt export-types format)) pathname elements)))))

(comtab:defmenu-command :export-selection ((window editor-window))
  (multiple-value-bind (graph-window selection) (:selection window)
    (commence-export-elements window selection "Export Selection")))

(comtab:defmenu-command :export ((window editor-window))
  (commence-export-elements window (graph-elements (cg:selected-window window)) "Export"))

;;; GIF probably won't work for a bit, so now we can simply get on with handling the
;;; PostScript device drawing. This will more or less the same, as far as the interface
;;; is concerned, but we'll be a bit more careful about graph handling and similar kinds
;;; of things. One important difference is the handling of colour. Colours need to be
;;; drawn from a vector so we can use them properly. This means more changes to the 
;;; colour system, although they aren't as bad as the last lot! Mainly, all we need is
;;; a way to get from a colour number to a real colour, so that we can always use an
;;; RGB code in the PostScript text. 

(defclass postscript-graph (graph ps:encapsulated-postscript-device)
  ())

(defmethod graph-interactive-p ((graph postscript-graph))
  nil)

;;; The actual graph area should be calculated from the exported elements, not simply
;;; taken for granted. So we do that next. 
;;;
;;; While this lot is going on, we shouldn't redraw the screen, so we shouldn't handle
;;; any update calls. However, this is hard to do with Common Graphics. The best solution
;;; is simply to call cg:process-pending-events before we do any exporting. 
;;;
;;; The bounding box can be calculated from the areas of all the elements. We should 
;;; handle the elements rather like the "cut" though, and include all elements dependent
;;; on those selected. 

(defun elements-area (elements)
  (let ((area '#.(cg:make-box 0 0 0 0)))
    (cg:ncopy-box area (graph-element-area (first elements)))
    (loop for element in (rest elements)
          do (nbox-union area (graph-element-area element)))
    (nbox-inset area -1 -1)
    area))

(defmethod get-bounding-box ((graph graph) elements)
  (elements-area elements))

(defmethod get-bounding-box ((graph postscript-graph) elements)
  (let ((box (call-next-method)))
    ;; (setf (cg:box-left box) 0)
    box))

(defmethod cg:device-open ((stream postscript-graph) options &aux box)
  (let ((elements (get-graph-elements-from-forms (getf options :terms) stream)))
    (setf box (get-bounding-box stream elements))
    (call-next-method stream
      `(:left-margin ,(cg:box-left box) :top-margin 0
        :right-margin ,(cg:box-right box) :bottom-margin ,(cg:box-height box)
        :page-width ,(cg:box-right box) :page-height ,(cg:box-bottom box)
        ,@options))
    (loop for element in elements
          do (set-window element stream))
    (setf (graph-elements stream) elements)
    stream))

(defgeneric export-elements (window format pathname elements))

(defmethod export-elements ((window editor-window) (format (eql :eps)) pathname elements)
  (complete-element-editor (editor-window-editor window) :end)
  (unless elements
    (return-from export-elements nil))
  (cg:process-pending-events)
  (call-next-method))

(defmacro with-resolution-variables (&body body)
  `(let ,(loop for variable in *screen-values*
               collect (list variable nil))
      ,@body))

(defparameter *eps-font* :helvetica)
(defvar *eps-resolution* 72)
(defvar *eps-font-correction* -1.5)
   
(defmethod export-elements ((window application-window) (format (eql :eps)) pathname elements)
  (cg:with-mouse-captured (cg:*screen*)
    (cg:set-cursor cg:*screen* cg:waiting-cursor)
    (let ((terms (loop for (element graph-element) in (get-file-elements elements)
                       collect element)))
      (with-resolution-variables
        (let ((*table-font-face* *eps-font*))
          (initialise-layout-parameters :resolution *eps-resolution*)
          (with-open-stream (output (cg:open-stream 'postscript-graph pathname :output
                                      :if-does-not-exist :create
                                      :if-exists :supersede
                                      :font-correction *eps-font-correction*
                                      :terms terms))
            (print-graph window output elements)))))))

