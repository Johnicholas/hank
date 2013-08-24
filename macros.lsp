;;; -*- Mode: Lisp; Package: HANK -*-
;;;
;;; Authors: Stuart Watt
;;;          The Open University

(in-package "HANK")

;;; A new macros file, so that macros are available right at the
;;; beginning, even if the functions that they expand into aren't.
;;; Beware of functions which they call during the expansion. 

(defmacro with-saved-font ((stream) &body body)
  (let ((stream-sym (gensym))
        (font-handle-sym (gensym)))
    `(let* ((,stream-sym ,stream)
            (,font-handle-sym (cg:font-handle ,stream-sym)))
       (unwind-protect (progn 
                         ,@body)
         (cg:set-font ,stream-sym ,font-handle-sym)))))

;;; A macro to save and restore the background colour for a 
;;; stream. This allows us to change the background colour
;;; (as we need to do for icons, for instance) without handling
;;; save and restore explicitly. 

(defmacro with-saved-background-colour ((window) &body body)
  (let ((coloursym (gensym))
        (windowsym (gensym)))
    `(let* ((,windowsym ,window)
            (,coloursym (cg:background-color ,windowsym)))
       (unwind-protect (progn ,@body)
         (cg:set-background-color ,windowsym ,coloursym)))))

(defmacro with-saved-foreground-colour ((window) &body body)
  (let ((coloursym (gensym))
        (windowsym (gensym)))
    `(let* ((,windowsym ,window)
            (,coloursym (cg:foreground-color ,windowsym)))
       (unwind-protect (progn ,@body)
         (cg:set-foreground-color ,windowsym ,coloursym)))))

;;; A similar macro which does basically the same for the paint
;;; operation. 

(defmacro with-saved-paint-operation ((window) &body body)
  (let ((operationsym (gensym))
        (windowsym (gensym)))
    `(let* ((,windowsym ,window)
            (,operationsym (cg:paint-operation ,windowsym)))
       (unwind-protect (progn ,@body)
         (cg:set-paint-operation ,windowsym ,operationsym)))))

;;; Sometimes a layout should be adjusted with its drawing turned
;;; off, and then redisplayed. This may be faster and better than
;;; incrementally managing everything. We should test to see if
;;; the drawing flag is on before doing any drawing. 

(defmacro with-delayed-display ((window) &body body)
  (let ((canvassym (gensym))
        (old-drawing-p (gensym)))
    `(let* ((,canvassym ,window)
            (,old-drawing-p (canvas-drawing-p ,canvassym)))
       (when ,old-drawing-p
         (setf (canvas-drawing-p ,canvassym) nil))
       (unwind-protect (progn ,@body)
         (when ,old-drawing-p
           (setf (canvas-drawing-p ,canvassym) t)
           (cg:redisplay-window ,canvassym))))))

(defmacro with-no-display ((window) &body body)
  (let ((canvassym (gensym))
        (old-drawing-p (gensym)))
    `(let* ((,canvassym ,window)
            (,old-drawing-p (canvas-drawing-p ,canvassym)))
       (when ,old-drawing-p
         (setf (canvas-drawing-p ,canvassym) nil))
       (unwind-protect (progn ,@body)
         (when ,old-drawing-p
           (setf (canvas-drawing-p ,canvassym) t))))))

;;; Revise with-clipping-box so that it is actually nestable. This needs
;;; a little care on the handling of windows. This is because each window
;;; may have its own clipping box. And above this, we should make it possible
;;; to nest with-clipping-box recursively. 
;;;
;;; Clipping is just getting to be too much of a pain. We should unify the
;;; with-clipping-box and with-nested-clipping-box macros, so that they always
;;; work. 

(defvar *clipping-top-level-p* t)

;;; Recursive calls to procedures using this macro can trample on the same 
;;; clipping boxes. There isn't a good solution to this problem. New boxes
;;; are really needed for each invocation in level. 

(defmacro with-clipping-box ((window box) &body body)
  (let ((windowsym (gensym))
        (scroll-position-sym (gensym))
        (clipping-box-sym (gensym))
        (boxsym (gensym)))
    `(let* ((,windowsym ,window)
            (,clipping-box-sym (if *clipping-top-level-p*
                                   '#.(cg:make-box -32768 -32768 32767 32767)
                                   (cg:nclipping-box ,windowsym ',(cg:make-box 0 0 0 0))))
            (,boxsym (cg:ncopy-box ',(cg:make-box 0 0 0 0) ,box))
            (*clipping-top-level-p* nil))
       (declare (special *clipping-top-level-p*))
       (unwind-protect (progn (cg:set-clipping-box ,windowsym ,boxsym)
                         ,@body)
         (cg:set-clipping-box ,windowsym ,clipping-box-sym)))))

;;; A variant on the above which is nestable. This restores the original
;;; clipping box. We can't use this above because at top level we end up
;;; setting the clipping region and failing to restore it appropriately.
;;; The problem is that the clipping region is faked to be small in scrolling
;;; and when we restore it, because we restored it, it stays small. 
;;; If we hadn't touched it, there wouldn't be a problem. This is a CG funny,
;;; and is probably a bug. 
;;;
;;; Only use this within the dynamic context of with-clipping-box. 

;;; (defmacro with-nested-clipping-box ((window box) &body body)
;;;   (let ((windowsym (gensym))
;;;         (scroll-position-sym (gensym))
;;;         (clipping-box-sym (gensym))
;;;         (boxsym (gensym)))
;;;     `(let* ((,windowsym ,window)
;;;             (,scroll-position-sym (cg:nscroll-position ,windowsym ',(cg:make-position 0 0)))
;;;             (,clipping-box-sym (cg:nclipping-box ,windowsym ',(cg:make-position 0 0)))
;;;             (,boxsym (cg:ncopy-box ',(cg:make-box 0 0 0 0) ,box)))
;;;        (cg:nposition* ,scroll-position-sym -1 -1)
;;;        (cg:nbox-move ,boxsym ,scroll-position-sym)
;;;        (unwind-protect (progn (cg:set-clipping-box ,windowsym ,boxsym)
;;;                          ,@body)
;;;          (cg:set-clipping-box ,windowsym ,clipping-box-sym)))))

(defmacro with-restart-filter (function &body body)
  `(let ((*restart-filters* (cons ,function *restart-filters*)))
     (declare (special *restart-filters*))
     ,@body))

