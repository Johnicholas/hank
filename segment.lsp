;;; -*- Mode: Lisp; Package: HANK -*-
;;;
;;; Author: Stuart Watt
;;;         Open University
;;;
;;; Design: Paul Mulholland
;;;         Stuart Watt
;;;         Open University

(in-package "HANK")

;;; OK, we're now at the point where we need to do the Manhattan calculations, 
;;; bearing in mind that this needs to work for both the drag and for graph elements
;;; created during the drag. These elements need to refer to both from and to
;;; elements, both with attachment points. Optionally, during the drag, the to
;;; element and attachment point may be missing, replaces by a position instead.
;;; The calculation can then be a bit different. Note that we can use the 
;;; attachment point's nsignum value (which can be calculated at any time) for
;;; some of the direction calculations. 

(defun create-segment ()
  (list* 0 nil (loop for i from 1 to 6 collect (cg:make-position 0 0))))

(defun nmake-segment (segment from-position from-point to-position to-point)
  (let ((from-signum (attachment-point-nsignum from-point '#.(cg:make-position 0 0)))
        (to-signum (when to-point
                     (attachment-point-nsignum to-point '#.(cg:make-position 0 0))))
        (from-intermediate '#.(cg:make-position 0 0))
        (to-intermediate '#.(cg:make-position 0 0))
        (temporary '#.(cg:make-position 0 0))
        (points (rest (rest segment)))
        fx fy tx ty fsx fsy tsx tsy dx dy mx my)
     
    (cg:ncopy-position temporary from-signum)
    (cg:nposition* temporary *attachment-point-standoff* *attachment-point-standoff*)
    (cg:ncopy-position from-intermediate from-position)
    (cg:nposition+ from-intermediate temporary)
     
    (when to-point
      (cg:ncopy-position temporary to-signum)
      (cg:nposition* temporary *attachment-point-standoff* *attachment-point-standoff*)
      (cg:ncopy-position to-intermediate to-position)
      (cg:nposition+ to-intermediate temporary))
    
    ;; Now we can simply go through the alternative cases. This will probably
    ;; take some time, but careful handling of signums may help it all come right
    ;; in the end. This will require thought. One fairly easy way of doing this
    ;; is to use a kind of turtle system. After all, we know the number of 
    ;; segments in total. 
     
    (setf fx (cg:position-x from-position))
    (setf fy (cg:position-y from-position))
    (setf tx (cg:position-x to-position))
    (setf ty (cg:position-y to-position))
    (setf dx (pro:i- tx fx))
    (setf dy (pro:i- ty fy))
    (setf mx (pro:i/ (pro:i+ fx tx) 2))
    (setf my (pro:i/ (pro:i+ fy ty) 2))
    (setf fsx (cg:position-x from-signum))
    (setf fsy (cg:position-y from-signum))
    (when to-point
      (setf tsx (cg:position-x to-signum))
      (setf tsy (cg:position-y to-signum)))
     
    (cond (to-point
           (cond ((not (pro:izerop (pro:i* fsy tsy)))
                  (if (and (pro:i= -1 (pro:i* fsy tsy))
                           (if (pro:i= 1 fsy) (pro:i>= fy ty) (pro:i<= fy ty)))
                      (progn
                        (setf (first segment) 0)
                        (cg:ncopy-position (nth 0 points) from-position)
                        (cg:ncopy-position (nth 1 points) from-intermediate)
                        (cg:nmake-position (nth 2 points) mx (cg:position-y from-intermediate))
                        (cg:nmake-position (nth 3 points) mx (cg:position-y to-intermediate))
                        (cg:ncopy-position (nth 4 points) to-intermediate)
                        (cg:ncopy-position (nth 5 points) to-position))
                      (let ((ay (cond ((and (pro:i= 1 fsy) (pro:i= 1 tsy))
                                       (pro:imax (cg:position-y from-intermediate)
                                                 (cg:position-y to-intermediate)))
                                      ((and (pro:i= -1 fsy) (pro:i= -1 tsy))
                                       (pro:imin (cg:position-y from-intermediate)
                                                 (cg:position-y to-intermediate)))
                                      (t my))))
                        (setf (first segment) 2)
                        (cg:ncopy-position (nth 2 points) from-position)
                        (cg:nmake-position (nth 3 points) fx ay)
                        (cg:nmake-position (nth 4 points) tx ay)
                        (cg:ncopy-position (nth 5 points) to-position))))
                 ((not (pro:izerop (pro:i* fsx tsx)))
                  (if (and (pro:i= -1 (pro:i* fsx tsx))
                           (if (pro:i= 1 fsx) (pro:i>= fx tx) (pro:i<= fx tx)))
                      (progn
                        (setf (first segment) 0)
                        (cg:ncopy-position (nth 0 points) from-position)
                        (cg:ncopy-position (nth 1 points) from-intermediate)
                        (cg:nmake-position (nth 2 points) (cg:position-x from-intermediate) my)
                        (cg:nmake-position (nth 3 points) (cg:position-x to-intermediate) my)
                        (cg:ncopy-position (nth 4 points) to-intermediate)
                        (cg:ncopy-position (nth 5 points) to-position))
                      (let ((ax (cond ((and (pro:i= 1 fsx) (pro:i= 1 tsx))
                                       (pro:imax (cg:position-x from-intermediate)
                                                 (cg:position-x to-intermediate)))
                                      ((and (pro:i= -1 fsx) (pro:i= -1 tsx))
                                       (pro:imin (cg:position-x from-intermediate)
                                                 (cg:position-x to-intermediate)))
                                      (t mx))))
                        (setf (first segment) 2)
                        (cg:ncopy-position (nth 2 points) from-position)
                        (cg:nmake-position (nth 3 points) ax fy)
                        (cg:nmake-position (nth 4 points) ax ty)
                        (cg:ncopy-position (nth 5 points) to-position))))
                 ((and (pro:i= -1 (pro:i* fsx tsy))
                       (if (pro:i= 1 fsx) 
                           (and (pro:i< fx tx) (pro:i< fy ty))
                           (and (pro:i> fx tx) (pro:i> fy ty))))
                  (setf (first segment) 3)
                  (cg:ncopy-position (nth 3 points) from-position)
                  (cg:nmake-position (nth 4 points) tx fy)
                  (cg:ncopy-position (nth 5 points) to-position))
                 ((and (pro:i= 1 (pro:i* fsx tsy))
                       (if (pro:i= 1 fsx) 
                           (and (pro:i< fx tx) (pro:i> fy ty))
                           (and (pro:i> fx tx) (pro:i< fy ty))))
                  (setf (first segment) 3)
                  (cg:ncopy-position (nth 3 points) from-position)
                  (cg:nmake-position (nth 4 points) tx fy)
                  (cg:ncopy-position (nth 5 points) to-position))
                 ((and (pro:i= -1 (pro:i* fsy tsx))
                       (if (pro:i= 1 fsy) 
                           (and (pro:i< fx tx) (pro:i< fy ty))
                           (and (pro:i> fx tx) (pro:i> fy ty))))
                  (setf (first segment) 3)
                  (cg:ncopy-position (nth 3 points) from-position)
                  (cg:nmake-position (nth 4 points) fx ty)
                  (cg:ncopy-position (nth 5 points) to-position))
                 ((and (pro:i= 1 (pro:i* fsy tsx))
                       (if (pro:i= 1 fsy)
                           (and (pro:i> fx tx) (pro:i< fy ty))
                           (and (pro:i< fx tx) (pro:i> fy ty))))
                  (setf (first segment) 3)
                  (cg:ncopy-position (nth 3 points) from-position)
                  (cg:nmake-position (nth 4 points) fx ty)
                  (cg:ncopy-position (nth 5 points) to-position))
                 ((not (pro:izerop (pro:i* fsx tsy)))
                  (setf (first segment) 1)
                  (cg:ncopy-position (nth 1 points) from-position)
                  (cg:ncopy-position (nth 2 points) from-intermediate)
                  (cg:nmake-position (nth 3 points) (cg:position-x from-intermediate) (cg:position-y to-intermediate))
                  (cg:ncopy-position (nth 4 points) to-intermediate)
                  (cg:ncopy-position (nth 5 points) to-position))
                 ((not (pro:izerop (pro:i* fsy tsx)))
                  (setf (first segment) 1)
                  (cg:ncopy-position (nth 1 points) from-position)
                  (cg:ncopy-position (nth 2 points) from-intermediate)
                  (cg:nmake-position (nth 3 points) (cg:position-x to-intermediate) (cg:position-y from-intermediate))
                  (cg:ncopy-position (nth 4 points) to-intermediate)
                  (cg:ncopy-position (nth 5 points) to-position))
                 (t
                  (error "Internal error"))))
          (t
           (setf (first segment) 4)
           (cg:ncopy-position (nth 4 points) from-position)
           (cg:ncopy-position (nth 5 points) to-position)))
    segment))

