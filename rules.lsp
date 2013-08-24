;;; -*- Mode: Lisp; Package: HANK -*-
;;;
;;; Author: Stuart Watt
;;;         Open University
;;;
;;; Design: Paul Mulholland
;;;         Stuart Watt
;;;         Open University

(in-package "HANK")

;;; The rules window. This window will contain the house rules in a readable and
;;; highlightable form. This should look a bit like a standard text editor, except
;;; that we can see the different steps being highlighted as we step through the
;;; rules. The source for the rules needs to be looked after with a little care,
;;; but basically, it should be a bit like the rest of Hank. In this window, we'll
;;; allow scrolling and a bunch of other stuff, just. Rules should be represented
;;; as highlightable elements in a list, so as the interpreter steps, rules can be
;;; walked through. 
;;;
;;; Lines are represented as Hank text forms, with the predicate representing some
;;; stylistic information. Each rule is represented as a list of lines. Finally,
;;; we do a few little bits of automatic layout and stuff, at run time. There are
;;; also a few tricks we use to handle things. 

(defmacro rule-name (rule)
  `(compound-operator ,rule))
(defmacro rule-length (rule)
  `(first (compound-arguments ,rule)))
(defmacro rule-lines (rule)
  `(rest (compound-arguments ,rule)))

(defclass rules-pane (cg:non-refreshing-pane)
  ((rules
     :initarg :rules
     :reader rules-pane-rules)
   (selected-lines
     :initform '(13 18)
     :reader rules-pane-selected-lines)))

(defun get-rule-lines (window)
  (max 0 (- (1- (loop for rule across (rules-pane-rules window)
                      summing (rule-length rule)))
            (floor (cg:box-height (cg:nvisible-box window '#.(cg:make-box 0 0 0 0)))
                   (cg:line-height window)))))

(defmethod cg:device-open ((window rules-pane) options)
  (prog1 (call-next-method)
    (cg:set-line-height window (cg:font-height (cg:nfontmetrics window '#.(cg:make-fontmetrics))))
    (cg:set-scroll-range window 0 (* (cg:line-height window) (get-rule-lines window)))))

(defmethod cg:user-scroll ((window rules-pane) scroll-type new-position)
   (case scroll-type
      ((:thumb-continuous :thumb-finished) 
       ;; scroll to position  
       (if (or (eq scroll-type :left) (eq scroll-type :right))
           (cg:scroll-to window new-position)
           (let ((line-height (cg:line-height window)))
             (setf (cg:position-y new-position)
                     (acl:i* line-height (acl:i/ (acl:i+ (cg:position-y new-position)
                                                         (acl:i/ line-height 2))
                                                 line-height)))
             (cg:scroll-to window new-position))))
      (t 
         (let* ((horizontal-p 
                   (or (eq scroll-type :left) (eq scroll-type :right)))
                (scroll-distance 
                 (case new-position
                    (:character 
                     (if horizontal-p (cg:space-width window) (cg:line-height window)))
                    (t ;; page  
                     (if horizontal-p 
                         (acl:i- (cg:visible-box-width window) (cg:space-width window)) 
                         (acl:i- (cg:visible-box-height window) (cg:line-height window)))))))
            (if horizontal-p 
               (cg:scroll window 
                  (cg:nmake-position #.(cg:make-position 0 0) 
                     (if (eq scroll-type :left)
                         (acl:i- scroll-distance) 
                         scroll-distance)
                     0))
               (cg:scroll window 
                  (cg:nmake-position #.(cg:make-position 0 0) 0 
                     (if (eq scroll-type :up)
                         (acl:i- scroll-distance) 
                         scroll-distance))))))))

(defmethod pc::update-scroll-bars-for-new-window-size ((window rules-pane))
   (let* ((visible-box (cg:nvisible-box window #.(cg:make-box 0 0 0 0))) 
	  (visible-box-width (cg:box-width visible-box)) 
	  (visible-box-height (cg:box-height visible-box)) 
	  (scroll-range-x (acl:i- (cg:page-width window t) visible-box-width)) 
	  (scroll-range-y (* (cg:line-height window) (get-rule-lines window)))
	  (scroll-position (cg:nscroll-position window #.(cg:make-position 0 0))) 
	  (scroll-position-x (cg:position-x scroll-position)) 
	  (scroll-position-y (cg:position-y scroll-position)))
      (cg:set-scroll-range window scroll-range-x scroll-range-y
         visible-box-width visible-box-height)
      ;; may have expanded window too much - scroll it back 
      (cg:scroll-to window 
	 (cg:nmake-position #.(cg:make-position 0 0) 
	    (acl:imin scroll-position-x scroll-range-x) 
	    (acl:imin scroll-position-y scroll-range-y)))))

;;; The main trick is to know what to draw. We could represent everything as a 
;;; list but that's a bit slow, so we'll do things a bit faster by knowing the length
;;; of the rules, in lines. 

(defmethod cg:redisplay-window ((window rules-pane) 
                                &optional (box (cg:nvisible-box window '#.(cg:make-box 0 0 0 0))))
  (cg:erase-contents-box window box)
  (let* ((line-height (cg:line-height window))
         (first-line (pro:i/ (cg:box-top box) line-height))
         (last-line (pro:i/ (pro:i+ (cg:box-bottom box) (pro:i1- line-height)) line-height))
         (selection (rules-pane-selected-lines window)))
    (loop with offset = 0
          for rule across (rules-pane-rules window)
          for rule-length = (rule-length rule)
          do (cond ((pro:i>= first-line rule-length)
                    (pro:idecf first-line rule-length)
                    (pro:idecf last-line rule-length)
                    (pro:iincf offset rule-length))
                   ((pro:i<= last-line 0)
                    (return))
                   (t
                    (let ((rule-first-line first-line)
                          (rule-last-line (pro:imin (pro:i1- rule-length) last-line))
                          (cell-box (cg:ncopy-box '#.(cg:make-box 0 0 0 0) box)))
                      (loop for line-number from rule-first-line to rule-last-line
                            for selectedp = (and (pro:i>= (pro:i+ offset line-number) (first selection))
                                                 (pro:i< (pro:i+ offset line-number) (second selection)))
                            do (setf (cg:box-top cell-box) (pro:i* line-height (pro:i+ offset line-number)))
                               (setf (cg:box-bottom cell-box) (pro:i+ line-height (cg:box-top cell-box)))
                               (when selectedp
                                 (cg:set-background-color window (pc::system-color win:color_highlight))
                                 (cg:erase-contents-box window cell-box)
                                 (cg:set-foreground-color window cg:white))
                                 
                               (draw-line window rule line-number
                                 (cg:nmake-position '#.(cg:make-position 0 0)
                                   (cg:box-left cell-box) (cg:box-top cell-box)))
                        
                               (when selectedp
                                 (cg:set-background-color window cg:white)
                                 (cg:set-foreground-color window cg:black)))
                      (pro:iincf offset rule-length)
                      (setf first-line 0)))))))

(defclass rules-window (cg:frame-with-single-child system-window)
  ()
  (:default-initargs :user-scrollable :vertical))

(defmethod cg:default-pane-class ((window rules-window))
  'rules-pane)

(defmethod draw-line ((window rules-pane) rule line-number position &aux rules)
  (setf rules (rules-pane-rules window))
  (setf (cg:position-x position) 2)
  (cg:move-to window position)
  (when (pro:izerop line-number)
    (cg:set-font window *rule-font-bold*)
    (format window "~A." (pro:i1+ (position rule rules)))
    (cg:set-font window *rule-font*))
  (setf (cg:position-x position) 30)
  (cg:move-to window position)
  (labels ((write-element (element)
             (etypecase element
               (null nil)
               (string (write-string element window))
               (list
                 (loop for element in element
                       do (write-element element)))
               (number
                 (setf (cg:position-x position) element)
                 (cg:move-to window position))
               (compound
                 (let ((operator (compound-operator element)))
                   (cond ((pro:whole-string-equal operator "Rule")
                          (write-element 
                            (format nil "~D" (1+ (position (first (compound-arguments element))
                                                   rules
                                                   :key #'compound-operator
                                                   :test #'pro:whole-string-equal)))))
                         ((pro:whole-string-equal operator "Bold")
                          (let ((old-font (cg:font window)))
                            (cg:set-font window *rule-font-bold*)
                            (loop for element in (compound-arguments element)
                                  do (write-element element))
                            (cg:set-font window old-font)))
                         ((pro:whole-string-equal operator "Italic")
                          (let ((old-font (cg:font window)))
                            (cg:set-font window *rule-font-italic*)
                            (loop for element in (compound-arguments element)
                                  do (write-element element))
                            (cg:set-font window old-font)))))))))
    (write-element (nth line-number (rule-lines rule)))))

;;; The working directory is probably a bit more important than it might
;;; seem. However, we can't simply use the Windows working directory, 
;;; because things can move. 

(defun open-rules-window ()
  (cg:open-stream 'rules-window cg:*screen* :io
    :font *rule-font*
    :rules (map 'vector #'identity (get-file-terms "c:\\stuart\\hank\\house.rul"))))

