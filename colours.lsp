;;; -*- Mode: Lisp; Package: HANK -*-
;;;
;;; Authors: Stuart Watt
;;;          The Open University

(in-package "HANK")

;;; Colour handling for the PC Ousel workbench. The concepts
;;; and approaches here have been borrowed from the colour
;;; transform system originally in the UNIX version, but a 
;;; number of changes have been made. 
;;;
;;; First, we are assuming colour/greyscale for now. Monochrome
;;; requires integrating stipples, and we don't want to do that.
;;;
;;; Second, we are a bit more careful in the handling of consing
;;; and in the caching of transforms. We give each window its
;;; own private transform table. This is not too desperately
;;; inefficient, as in practice there will never be that many 
;;; matches involved in any given window. Besides, it is a 
;;; caching trick anyway. One important change is that we do allow
;;; the window's cache to be cleared more easily. This will be 
;;; used when we do things like change the label.
;;;
;;; Additional colour handling to make the colour system on the PC work
;;; properly. This is tricky, but it is probably the best I can do for
;;; now. We assume 256 colour mode, rather than simple VGA mode. I am
;;; not sure this is right, but it is at least a good start.
;;;
;;; If we aren't in RGB mode, we use a palette of colours for colour
;;; drawing. This palette is filled on demand, for the colours that
;;; we need. We have a search function which finds colours that are
;;; close to the one that we are looking for. In our colour palette we
;;; use RGB codes of between 0.0 and 1.0. 

(defstruct (colour (:type list)
                   (:constructor make-colour (red green blue)))
  red
  green
  blue)

;;; We have an additional set of colours, for the current foreground
;;; and background colours. These are what will usually be transformed
;;; to provide real colours. These are specials, so they can be rebound
;;; in a dynamic context. These values may well not be best, but different
;;; values can be set in the globals file which is loaded first.
;;;
;;; hls-to-rgb converts a triple for hue, lightness, and saturation
;;; back into RGB terms which can then be used for display.

(defun hls-to-rgb (hue lightness saturation)
  (declare (type (number 0 360) hue))
  (declare (type (number 0 1) saturation value))
  (flet ((value (n1 n2 hue)
           (when (> hue 360.0) (setf hue (- hue 360.0)))
           (when (< hue 0.0) (setf hue (+ hue 360.0)))
           (cond ((< hue 60.0) (+ n1 (* (/ hue 60.0) (- n2 n1))))
                 ((< hue 180.0) n2)
                 ((< hue 240.0) (+ n1 (* (/ (- 240.0 hue) 60.0) (- n2 n1))))
                 (t n1))))
    (let* ((m2 (if (<= lightness 0.5)
                   (* lightness (+ 1.0 saturation))
                   (- (+ lightness saturation) (* lightness saturation))))
           (m1 (- (* 2.0 lightness) m2)))
      (if (= saturation 0.0)
          (values lightness lightness lightness)
          (values (value m1 m2 (+ hue 120.0))
                  (value m1 m2 hue)
                  (value m1 m2 (- hue 120.0)))))))

;;; rgb-to-hls converts an RGB colour into a hue, lightness, and
;;; saturation triple.

(defun rgb-to-hls (red green blue)
  (declare (type (number 0 1) red green blue))
  (let* ((max (max red green blue))
         (min (min red green blue))
         (lightness (/ (+ max min) 2.0)))
    (if (= max min)
        (values 0.0 lightness 0.0)
        (let* ((difference (- max min))
               (saturation (if (<= lightness 0.5)
                               (/ difference (+ max min))
                               (/ difference (- 2.0 max min)))))
          (let ((rc (/ (- max red) difference))
                (gc (/ (- max green) difference))
                (bc (/ (- max blue) difference)))
            (values (let ((hue (cond ((= red max) (- bc gc))
                                     ((= green max) (+ 2.0 rc (- bc)))
                                     ((= blue max) (+ 4.0 gc (- rc))))))
                      (setf hue (* hue 60.0))
                      (when (< hue 0.0)
                        (incf hue 360.0))
                      hue)
                    lightness
                    saturation))))))

;;; There is one irreversible translation, when the RGB values are all equal, 
;;; representing a grey, the hue cannot be determined, it is set to zero. The 
;;; reverse translation for a grey will not preserve the hue as it was 
;;; before. Usually this does not matter.

(defun colour-hls (colour)
  (multiple-value-call #'rgb-to-hls (values-list colour)))

(defun make-colour-hls (hue lightness saturation)
  (multiple-value-call #'make-colour
    (hls-to-rgb hue lightness saturation)))

;;; Modifying the hls code to implement light and shade is fairly
;;; simple. In both cases we want to decrease the saturation, and
;;; then we alter the value.

(defun colour-intensity (colour factor)
  (multiple-value-call #'make-colour-hls
    (multiple-value-bind (hue lightness saturation) (colour-hls colour)
      (values
        hue
        (expt lightness (/ 1 factor))
        (* saturation (min factor (/ 1 factor)))))))

;;; Tinting is rather more problematic. We ideally want white to
;;; take on the other colour wholesale, and black to be unchanged,
;;; ie. the amount of tinting depends on the lightness. Try the
;;; following...

(defun colour-tint (colour1 colour2)
  (multiple-value-call #'make-colour-hls
    (multiple-value-bind (hue1 lightness1 saturation1) (colour-hls colour1)
      (multiple-value-bind (hue2 lightness2 saturation2) (colour-hls colour2)
        (values
          (+ (* hue1 (- 1.0 lightness1)) (* hue2 lightness1))
          (+ (* lightness1 (- 1.0 lightness1)) (* lightness2 lightness1))
          (+ (* saturation1 (- 1.0 lightness1)) (* saturation2 lightness1)))))))

;;; Finally, to interpolate between two colours use the following
;;; function. Although this is trivial, it can be amended if the
;;; results are not up to scratch.

(defun mean (number1 number2)
  (/ (+ number1 number2) 2))

(defun colour-blend (colour1 colour2)
  (make-colour (mean (colour-red colour1) (colour-red colour2))
	       (mean (colour-green colour1) (colour-green colour2))
	       (mean (colour-blue colour1) (colour-blue colour2))))

(defun make-rgb-from-colour (colour)
  (cg:make-rgb :red (colour-red colour) :green (colour-green colour) :blue (colour-blue colour)))

(defmacro define-colours (&rest colours)
  `(progn
     (defconstant *colour-source* ',colours)
     ,@(loop for colour in colours
             collect `(defvar ,(first colour)))
     (defconstant *colour-palette-vector*
       (vector ,@(loop for colour in colours
                       collect `',colour)))
     (defconstant *colour-vector*
       (vector ,@(loop for colour in colours
                       collect `',(make-rgb-from-colour (second colour)))))))

(define-colours
  (*white-colour* #.(make-colour 1.0 1.0 1.0))
  (*black-colour* #.(make-colour 0.0 0.0 0.0))
  (*title-colour* #.(make-colour-hls 0.0 0.75 0.0))
  (*pending-colour* #.(make-colour 0.9 0.1 0.1))
  (*header-colour* #.(make-colour-hls 0.0 0.875 0.0))
  (*variable-colour* #.(make-colour 0.0 0.0 0.6))
  (*grid-colour* #.(make-colour 0.0 1.0 1.0)))

#+ACLPC
(defun number-of-screen-colours ()
  (let ((hdc (pc:device-context cg:*screen*)))
    (win:getdevicecaps hdc win:sizepalette)))

;;; The palette should be created and initialised with black and white
;;; which we can then use in the icon system. 
;;;
;;; We can't initialise the colour system until later, when the
;;; screen has been properly created. 

#+ACLPC
(progn
  (defun initialise-windows-palette ()
    (let ((colours (number-of-screen-colours)))
      (setf *colour-palette* nil)
      (if (or (zerop colours) (> colours 256))
          (loop for binding across *colour-palette-vector*
                for colour = (second binding)
                do (setf (symbol-value (first binding)) (make-rgb-from-colour colour)))
          (let ((array (make-array colours :initial-element cg:white)))
            (loop for binding across *colour-palette-vector*
                  for colour = (second binding)
                  for index from 0
                  do (setf (symbol-value (first binding)) index)
                     (setf (aref array index) (make-rgb-from-colour colour)))
            (setq *colour-palette* (cg:open-palette cg:*screen* array ()))))))

  (defun clear-windows-palette (&rest ignore)
    (when *colour-palette*
      (cg:close-palette cg:*screen* *colour-palette*)
      (setf *colour-palette* nil)))
  
  (clear-windows-palette)
  (initialise-windows-palette)
  (unless (member 'initialise-windows-palette acl::*system-init-fns*)
    (setf acl::*system-init-fns* (nconc acl::*system-init-fns*
                                        '(initialise-windows-palette))))
  (push #'clear-windows-palette acl::*system-exit-fns*)
  )



