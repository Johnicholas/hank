;;; -*- Mode: Lisp; Package: HANK -*-
;;;
;;; Author: Stuart Watt
;;;         The Open University
;;;
;;; Design: Paul Mulholland
;;;         Stuart Watt
;;;         The Open University
;;;
;;; A whole bunch of useful Common Lisp extensions that people 
;;; might find handy. I use them quite a lot in the workbench
;;; but other people might find them handy for some things. 

(in-package "HANK")

(defun mapplist (function plist)
  (loop
    (when (endp plist)
      (return-from mapplist))
    (funcall function (first plist) (second plist))
    (setq plist (rest (rest plist)))))

;;; Getting cell data needs to be handled with a bit more care. If data doesn't
;;; exist, we need to create it. We don't need to do this, of course, when we're
;;; getting data, we can simply default. 

(defun replace-element (value list &rest indices)
  (cond ((null indices) value)
        (t
         (let ((index (first indices)))
           (unless (pro:i> (length list) index)
             (setf list (nconc list (make-list (pro:i1+ (pro:i- index (length list)))))))
           (setf (nth index list) (apply #'replace-element value (nth index list) (rest indices)))
           list))))

(defun frame-window-of-window (window)
  (loop if (null window)
          do (return nil)
        else if (typep window 'cg:frame-window)
          do (return window)
        else
          do (setf window (cg:stream-location window))))

;;; plist-equal is an equality test on plists, with a higher-order
;;; parameter just for fun. We use '#.(make-symbol "garble") to
;;; get a value which is unique, and so cannot be found in
;;; a plist. This is needed because otherwise '(:a nil) would
;;; match (), and that would never do. 

(defun plist-equal-one-way (plist1 plist2 test)
  (loop
    (let ((key (first plist1))
          (value (second plist1)))
      (unless (funcall test value (getf plist2 key '#.(make-symbol "garble")))
        (return-from plist-equal-one-way nil)))
    (setf plist1 (rest (rest plist1))))
  t)

(defun plist-equal (plist1 plist2 &key (test #'eql))
  (and (plist-equal-one-way plist1 plist2 test)
       (plist-equal-one-way plist2 plist1 test)))

;;; I keep on wanting to use multiple-value-setf, so I thought that I
;;; would write. Then I can use it without worrying too much.

(defmacro multiple-value-setf (places form)
  (let ((variables (map 'list #'(lambda (place)
                                  (declare (ignore place))
                                  (gensym))
                        places)))
    `(multiple-value-bind ,variables ,form
       (setf ,@(mapcan #'(lambda (variable place)
			   `(,place ,variable))
		       variables places)))))

(defmacro ascii-to-character (code)
  #+(or :ACLPC :PROCYON-COMMON-LISP) `(int-char ,code)
  #-(or :ACLPC :PROCYON-COMMON-LISP) `(code-char ,code))

(defmacro character-to-ascii (char)
  #+(or :ACLPC :PROCYON-COMMON-LISP) `(char-int ,char)
  #-(or :ACLPC :PROCYON-COMMON-LISP) `(char-code ,char))

;;; Some useful justification stuff, which allows boxes
;;; to be justified within one another. Two keys are passed
;;; to indicate the justification.

(defun nbox-justify (box1 box2 hjustify vjustify)
  (let ((width1 (cg:box-width box1))
        (width2 (cg:box-width box2))
        (height1 (cg:box-height box1))
        (height2 (cg:box-height box2))
        (left1 (cg:box-left box1))
        (left2 (cg:box-left box2))
        (top1 (cg:box-top box1))
        (top2 (cg:box-top box2))
        (offset '#.(cg:make-position 0 0)))
    (cg:nmake-position offset 0 0)
    (when hjustify
      (setf (cg:position-x offset) (pro:i- left2 left1))
      (incf (cg:position-x offset) (ccase hjustify
                                     (:left 0)
                                     (:right (pro:i- width2 width1))
                                     (:center (pro:i/ (pro:i- width2 width1) 2)))))
    (when vjustify
      (setf (cg:position-y offset) (pro:i- top2 top1))
      (incf (cg:position-y offset) (ccase vjustify
                                     (:top 0)
                                     (:bottom (pro:i- height2 height1))
                                     (:center (pro:i/ (pro:i- height2 height1) 2)))))
    (cg:nbox-move box1 offset)))

(defun box-justify (box1 box2 hjustify vjustify)
  (nbox-justify (cg:copy-box box1) box2 hjustify vjustify))

;;; Destructive versions of box corner accessing functions. These
;;; are used to save on consing in a few places, notably when
;;; handling graphics. 

(defun nbox-top-left (position box)
  (cg:nmake-position position (cg:box-left box) (cg:box-top box)))

(defun nbox-top-right (position box)
  (cg:nmake-position position (cg:box-right box) (cg:box-top box)))

(defun nbox-bottom-left (position box)
  (cg:nmake-position position (cg:box-left box) (cg:box-bottom box)))

(defun nbox-bottom-right (position box)
  (cg:nmake-position position (cg:box-right box) (cg:box-bottom box)))

(defun nbox-centre (position box)
  (let ((left (cg:box-left box))
        (top (cg:box-top box))
        (right (cg:box-right box))
        (bottom (cg:box-bottom box)))
    (cg:nmake-position position (pro:i/ (pro:i+ left right) 2) (pro:i/ (+ top bottom) 2))))

;;; Inset the box by a number of pixels in each dimension. This
;;; is actually a very useful function when it comes to handling
;;; some kinds of operation.

(defun nbox-inset (box dx dy)
  (cg:nmake-box box
    (pro:i+ (cg:box-left box) dx) (pro:i+ (cg:box-top box) dy)
    (pro:i- (cg:box-right box) dx) (pro:i- (cg:box-bottom box) dy)))

;;; Union two boxes destructively rather than construcively. This 
;;; is going to save us some consing in a few places.

(defun nbox-union (box1 box2)
  (cg:nmake-box box1
    (pro:imin (cg:box-left box1) (cg:box-left box2))
    (pro:imin (cg:box-top box1) (cg:box-top box2))
    (pro:imax (cg:box-right box1) (cg:box-right box2))
    (pro:imax (cg:box-bottom box1) (cg:box-bottom box2))))

(defun nbox-intersect (box1 box2)
  (cg:nmake-box box1
    (pro:imax (cg:box-left box1) (cg:box-left box2))
    (pro:imax (cg:box-top box1) (cg:box-top box2))
    (pro:imin (cg:box-right box1) (cg:box-right box2))
    (pro:imin (cg:box-bottom box1) (cg:box-bottom box2))))

(defun point-on-point-p (position source-position)
  (let ((dx (pro:iabs (pro:i- (cg:position-x position) (cg:position-x source-position))))
        (dy (pro:iabs (pro:i- (cg:position-y position) (cg:position-y source-position)))))
    (cond ((pro:i> dx *point-slop*) ())
          ((pro:i> dy *point-slop*) ())
          (t t))))

(defun point-on-line-p (position source-position destination-position)
  (let* ((x1 (cg:position-x source-position))
         (y1 (cg:position-y source-position))
         (x2 (cg:position-x destination-position))
         (y2 (cg:position-y destination-position))
         (x (cg:position-x position))
         (y (cg:position-y position)))
    (unless (and (pro:i>= (pro:i+ (pro:imax x1 x2) *line-slop*) x)
                 (pro:i>= x (pro:i- (pro:imin x1 x2) *line-slop*))
                 (pro:i>= (pro:i+ (pro:imax y1 y2) *line-slop*) y)
                 (pro:i>= y (pro:i- (pro:imin y1 y2) *line-slop*)))
      (return-from point-on-line-p nil))
    (let* ((dx (pro:i- x2 x1))
           (dy (pro:i- y2 y1))
           (d (pro:imin (pro:iabs (pro:i- y (if (pro:izerop dx) y1 (pro:i+ y1 (pro:i* dy (pro:i/ (pro:i- x x1) dx))))))
                        (pro:iabs (pro:i- x (if (pro:izerop dy) x1 (pro:i+ x1 (pro:i* dx (pro:i/ (pro:i- y y1) dy)))))))))
      (pro:i< d *line-slop*))))

;;; We offer a clean up function, which should be called on all text values entered
;;; through the interface. This removes leading and trailing spaces, and converts 
;;; all intermediate spaces to a single space. This means matching can be much
;;; simpler; that is, a simple case-insensitive comparison. In fact, we should be
;;; a bit careful; quoted strings might need to be handled with a bit more sensitivity. 

(defconstant *value-length* 1024)
(defconstant *value-buffer* (make-array *value-length* :fill-pointer 0 :element-type 'character))

(defun canonicalise-value (string)
  (setf (fill-pointer *value-buffer*) 0)
  (loop with beginp = t
        with pending-space-p = nil
        for character across string
        for spacep = (eql character #\Space)
        if (not spacep)
          if (and pending-space-p (not beginp))
            do (vector-push #\Space *value-buffer*)
          end
          and do (vector-push character *value-buffer*)
          and do (setf pending-space-p nil)
          and do (setf beginp nil)
        else
          do (setf pending-space-p t)
        end)
  (copy-seq *value-buffer*))

;;; Note that using getf is potentially unreliable as it only
;;; uses eq. As the name could be a string, we need some way
;;; of plisting things which are strings. In practice, we can
;;; use getf so long as we are sure that the name is eq to 
;;; any name already in the list. 
;;; 
;;; As a result, we write extra functions that are rather more reliable
;;; and controllable by allowing them to take a test function which
;;; is not required to be #'eq. Although these definitions will be
;;; slower, they will not be called that frequently that this matters.

(defun weak-getf (plist indicator &key (test #'eql) default)
  (loop
    (when (endp plist)
      (return-from weak-getf default))
    (when (funcall test indicator (first plist))
      (return-from weak-getf (second plist)))
    (setf plist (rest (rest plist)))))

(defun weak-set-getf (plist indicator value &key (test #'eql))
  (let ((whole-plist plist))
    (loop
      (when (endp plist)
        (return))
      (when (funcall test indicator (first plist))
        (setf (second plist) value)
        (return-from weak-set-getf whole-plist))
      (setf plist (rest (rest plist))))
    (list* indicator value whole-plist)))

;;; SNW: 13/5/92: Seriously "aaagh" bug in weak-remf-function.

(defun weak-remf-function (plist indicator &key (test #'eql))
  (when (endp plist)
    (return-from weak-remf-function nil))
  (when (funcall test indicator (first plist))
    (return-from weak-remf-function
      (weak-remf-function (rest (rest plist)) indicator :test test)))
  (list* (first plist) (second plist)
    (weak-remf-function (rest (rest plist)) indicator :test test)))

(define-modify-macro weak-remf (indicator &rest arguments) weak-remf-function)

(define-setf-method weak-getf (place indicator &key (test #'eql))
  (multiple-value-bind (temps values stores store-form access-form)
      (get-setf-method place)
    (let ((itemp (gensym))
          (ttemp (gensym))
          (store (gensym))
          (stemp (first stores)))
      (values (list* itemp ttemp temps)
              (list* indicator test values)
              (list store)
              `(let ((,stemp (weak-set-getf ,access-form ,itemp ,store :test ,ttemp)))
                 ,store-form
                 ,store)
              `(weak-getf ,access-form ,itemp :test ,ttemp)))))

;;; I'm starting to use the following form more and more, so I 
;;; thought I might as well add something which makes it a bit
;;; shorter right here. I would have defined this using
;;; define-modify-macro, but I can't because of the first 
;;; parameter not being a reference. 

(defmacro deletef (element reference &rest keys)
  `(setf ,reference (delete ,element ,reference ,@keys)))

;;; A string appending utility which allows us to call reduce 
;;; in a list of string elements without writing a special case
;;; all the time.

(defun string-concatenate (strings)
  (let ((length (length strings)))
    (cond ((pro:i= length 0) "")
          ((pro:i= length 1) (first strings))
          ((pro:i< length #.(- lambda-parameters-limit 3))
           (apply #'concatenate 'string strings))
          (t (reduce #'(lambda (string1 string2)
                         (concatenate 'string string1 string2))
                     strings)))))

(defun get-alist (environment keyword &optional default)
  (let ((result (assoc keyword environment)))
    (if result
        (rest result)
        default)))

(define-setf-method get-alist (environment keyword)
  (multiple-value-bind (temps vals stores store-form access-form)
      (get-setf-method environment)
    (let ((ktemp (gensym))
          (store (gensym))
          (stemp (first stores))
          (alisttemp (gensym)))
      (values (cons ktemp temps)
              (cons keyword vals)
              (list store)
              `(let* ((,alisttemp ,access-form)
                      (,stemp (assoc ,ktemp ,alisttemp)))
                 (if ,stemp
                     (setf (rest ,stemp) ,store)
                     (progn
                       (setf ,stemp (cons (cons ,ktemp ,store) ,alisttemp))
                       ,store-form
                       ,store)))
              `(get-alist ,access-form ,ktemp)))))

;;; To work out where we are in the right window, call this
;;; function. Note that this should work between "rooms" if
;;; at all possible.
;;;
;;; Performance tested: this function will run about 200 times
;;; a second. This is fine for dragging, since it should only
;;; be called once for every mouse-moved event. The point is
;;; that it shouldn't make dragging like "Great Apes In Treacle"
;;; (thanks for the quote, Ceri.)
;;;
;;; This has had to be made more complicated because it failed to
;;; take into account the scrolling position. This looks like a
;;; Common Graphics bug to me. The function at fault is
;;; cg:screen-to-window-units. We can't handle visible boxes
;;; for screens, so we need to detect and handle them differently.
;;;
;;; Rewritten to provide some kind of support for a few additional
;;; features needed to be kind to the user. These features include
;;; feedback during the drag and autoscrolling. Without these the
;;; drag and drop system will eventually get to be a bit of a 
;;; drag. Autoscrolling requires a little care, because we need
;;; to avoid autoscrolling sometimes, when we are dragging into
;;; and other window, but when we are dragging withion the same 
;;; window we needed. 

(defun get-window-and-position (screen-position)
  (let* ((position (cg:ncopy-position '#.(cg:make-position 0 0) screen-position))
         (window (or #+ACLPC (cg:which-window cg:*screen* position)
                     #+Procyon (which-window cg:*screen* position) cg:*screen*)))
    (unless (typep window 'cg:window)
      (return-from get-window-and-position (values window position)))
    (let ((visible-box (cg:nvisible-box window '#.(cg:make-box 0 0 0 0))))
      (cg:screen-to-window-units window position)
      (cg:nmake-position position
        (+ (cg:position-x position) (cg:box-left visible-box))
        (+ (cg:position-y position) (cg:box-top visible-box)))
      (values window position))))

;;; cg:which-window doesn't do anything except return nil. This is not 
;;; really very useful. Despite the fact that it will be slow, we need something
;;; which can do the same thing.

(defun which-window (device position)
  (let ((box #.(cg:make-box 0 0 0 0)))
    (loop
      (loop for window = (cg:front-window device) then (cg:next-window window)
            while window
            do (unless window
                 (return-from which-window (values device position)))
               (when (member (cg:window-state window) '(:normal :maximized))
                 (cg:nwindow-interior window box)
                 (when (cg:inside-box-p position box)
                   (pro:idecf (cg:position-x position) (cg:box-left box))
                   (pro:idecf (cg:position-y position) (cg:box-top box))
                   (setf device window)
                   (return nil)))
            finally do (return-from which-window (values device position))))))

(defun safe-read-from-string (string)
  #+ACLPC (ignore-errors (read-from-string string))
  #+Procyon-Common-Lisp 
    (let ((pro:*error-hook* #'(lambda (continue format arguments)
                                (return-from safe-read-from-string nil))))
      (pro:errorset (read-from-string string nil nil) nil)))

(defun copy-array (old-array)
  (let ((array (make-array (array-dimensions old-array)
                 :element-type (array-element-type old-array))))
    (loop for i from 0 below (array-total-size old-array)
          do (setf (row-major-aref array i) (row-major-aref old-array i)))
    array))

(defmacro variablep (item)
  `(let ((variable ,item))
     (and (stringp variable) 
          (or (pro:izerop (length variable))
              (and (eql #\? (char variable 0))
                   (eql #\? (char variable (pro:i1- (length variable)))))))))

;;; #+Macintosh
;;; (defconstant gray-pattern
;;;   (let ((pattern (ct:ccallocate tb:Pattern)))
;;;     (loop for i from 0 below 8
;;;           do (setf (ct:cref tb:Pattern pattern (tb:pat (fixnum i)))
;;;                      (nth (mod i 2) '(#xAA #x55))))
;;;     pattern))
;;; 
;;; #+Macintosh
;;; (defconstant black-pattern
;;;   (let ((pattern (ct:ccallocate tb:Pattern)))
;;;     (loop for i from 0 below 8
;;;           do (setf (ct:cref tb:Pattern pattern (tb:pat (fixnum i))) #xFF))
;;;     pattern))

;;; The Macintosh dot pattern isn't quite as fine as we need, so we hack it!
;;; This is a bit nasty, but it should work for a fair while. I am sure there
;;; is something neater that can be done at the CG level for this. I just hope
;;; these patterns aren't reset at image load time, or we'll need to do this as
;;; an initialisation. --SNW


#+Macintosh
(let ((pattern (mac::select-line-dashing-pattern :dot)))
  (loop for i from 0 below 8
        do (setf (ct:cref tb:Pattern pattern (tb:pat (fixnum i)))
                   (nth (mod i 2) '(#xAA #x55)))))

(defmacro set-drag-line (window)
  #+Windows `(cg:set-line-dashing ,window :dot)
  #+Macintosh `(cg:set-line-dashing ,window :dot)
  ;;; #+Macintosh `(progn
  ;;;                (tb:SetPort (mac:window-handle ,window))
  ;;;                (tb:PenPat gray-pattern))
  )

(defmacro unset-drag-line (window)
  #+Windows `(cg:set-line-dashing ,window :solid)
  #+Macintosh `(cg:set-line-dashing ,window :solid)
  ;;; #+Macintosh `(progn
  ;;;                (tb:SetPort (mac:window-handle ,window))
  ;;;                (tb:PenPat black-pattern))
  )

#+Macintosh
(defun key-pressed-p (virtual-key)
  (let* ((key-map (ct:ccallocate tb:KeyMap))
         (word (pro:i/ virtual-key 32))
         (offset (pro:i- virtual-key (pro:i* word 32)))
         (byte (pro:i/ offset 8)))
    (tb:GetKeys key-map)
    (logbitp (pro:i+ (pro:i* (pro:i- 3 byte) 8) (pro:imod offset 8))
      (ct:cref tb:KeyMap key-map ((fixnum word)) :static))))

(defun shift-key-pressed-p ()
  #+Macintosh (key-pressed-p 56)
  #+Windows (cg:key-is-down-p machine::vk-shift))

#+ACLPC
(progn
  (defconstant max-teb-length 32767)
  (acl::defbparameter *teb-length* 0)
  (acl::defbparameter *teb-index* 0)

  (acl::defbparameter *teb* (make-string max-teb-length))
  (pushnew '*teb* acl::*system-session-variables*)
  
  (push #'(lambda ()
            (setq *teb* (make-string max-teb-length)))
        acl::*system-init-fns*)
  
  (defmacro EW-POSITION (EC_Handle)
    `(PC:loword (WIN:SendMessage ,EC_Handle WIN::EM_GETSEL 0 0 :static)))
  
  (defmacro EW-GET-WINDOW-TEXT (EC_Handle)
    (let ((EC_Handle-var (if (atom EC_Handle) EC_Handle (gensym))))
      `(let (,@(if (eq EC_Handle-var EC_Handle) nil `((,EC_Handle-var ,EC_Handle))))
         (setf *teb-index* (EW-position ,EC_Handle-var)
               *teb-length* (win::GetWindowText ,EC_Handle-var *teb* 32767)))))
  
  (defmacro EW-LENGTH (EC_Handle)
    (let ((EC_Handle-var (if (atom EC_Handle) EC_Handle (gensym))))
      `(let* (,@(if (eq EC_Handle-var EC_Handle)
		    nil
		    `((,EC_Handle-var ,EC_Handle)))
              (temp 
               (WIN:SendMessage ,EC_Handle-var WIN::EM_LINEINDEX
                 (pro:i1- (WIN:SendMessage ,EC_Handle-var WIN::EM_GETLINECOUNT 0 0 :static))
                 0 :static)))
         (pro:i+ temp (WIN:SendMessage ,EC_Handle-var WIN::EM_LINELENGTH temp  0 :static)))))
  
  (defmacro EW-DELETE (EC_Handle)
    `(WIN:SendMessage ,EC_Handle WIN::WM_CLEAR 0 0 :static))
  
  (defmacro EW-HIGHLIGHT-RANGE (EC_Handle)
    `(let ((range (win:SendMessage ,EC_Handle 
                    (if (typep (pc::window-from-hwnd ,EC_Handle) 'pc::combo-box-pane) ;; <2>
                        win::CB_GETEDITSEL ;; <2>
                        win::EM_GETSEL)
                    0 0 :static)))
       (values (pc:loword range) (pc:hiword range)))))

#+Procyon-Common-Lisp
(progn
  (defmacro get-window-text (window)
    `(te::get-window-text ,window))
  (defmacro teb () 'mac::*teb*))

#+ACLPC 
(progn
  (defmacro get-window-text (window)
    `(ew-get-window-text (machine:window-handle ,window)))
  (defmacro teb () '*teb*)
  
  (defmacro ew-write-string (handle string from to)
    (let ((handle-var (if (atom handle) handle (gensym)))
          (string-var (if (atom string) string (gensym)))
          (from-var (if (atom from) from (gensym)))
          (to-var (if (atom to) to (gensym))))
      `(let (,@(unless (eq handle handle-var) `((,handle-var ,handle)))
             ,@(unless (eq string string-var) `((,string-var ,string)))
             ,@(unless (eq from from-var) `((,from-var ,from)))
             ,@(unless (eq to to-var) `((,to-var ,to))))
         (cond ((pro:izerop (length (the simple-string ,string-var))))
               ((pro:i= ,from-var ,to-var))
               ((eql ,to-var (length (the simple-string ,string-var)))
                (pc:sendmessage-with-pointer ,handle-var win:em_replacesel 0 
                  (ct:cref (char *) ,string-var ((fixnum ,from-var) ct:&)) :static t))
               (t
                (let ((old-char (char ,string-var ,to-var)))
                  (setf (char ,string-var ,to-var) '#.(int-char 0))
                  (pc:sendmessage-with-pointer ,handle-var win:em_replacesel 0 
                    (ct:cref (char *) ,string-var ((fixnum ,from-var) ct:&)) :static t)
                  (setf (char ,string-var ,to-var) old-char)))))))
  
  (defmacro ew-set-highlight-range (handle start end)
    (if (atom handle)
        `(progn
           (win:sendmessage ,handle win:em_setsel ,start ,end :static)
           (win:sendmessage ,handle win:em_scrollcaret 0 0 :static))
        (let ((hvar (gensym)))
          `(let ((,hvar ,handle))
             (win:sendmessage ,hvar win:em_setsel ,start ,end :static)
             (win:sendmessage ,hvar win:em_scrollcaret 0 0 :static))))))

;;; Borrowed from the source code, allow us to use the clipboard, but not for 
;;; text. That is, we can store data on the clipboard, but it can't be used
;;; by other programs. At least, that's what I hope win:cf_privatefirst 
;;; means. 

#+ACLPC
(progn
  (defconstant clphdata (ct:callocate win:handle))
  (defconstant clppdata (ct:callocate (:void *)))

  (defmethod cg:convert-clipboard-from-lisp ((window main-window) object format)
    (when (and (listp object) (every #'compound-p object))
      (setf object (with-output-to-string (output)
                     (write-terms-to-stream object output)))
      (let ((clpsize (pro:i1+ (ct:strlen object))))
        (unless (ct:null-handle-p win:handle (win:GlobalAlloc win:gmem_moveable clpsize clphdata))
          (win:GlobalLock clphdata clppdata)
          (unless (ct:null-cpointer-p clppdata)
            (sys:far-poke object clppdata 0 clpsize)
            (win:GlobalUnlock clphdata)
            (win:SetClipboardData win:cf_privatefirst clphdata :static))))
      (return-from cg:convert-clipboard-from-lisp))
    (call-next-method))

  (defmethod cg:convert-clipboard-to-lisp ((window main-window))
    ;; look for text and make a string
    (win:GetClipboardData win:cf_privatefirst clphdata)
    (when (ct:null-handle-p win:handle clphdata)
      (call-next-method)
      (return-from cg:convert-clipboard-to-lisp))
    (win:GlobalLock clphdata clppdata)  
    (unless (ct:null-cpointer-p clppdata)
      (let* ((raw-clpsize (win:GlobalSize clphdata :static))
	     (clpsize (if (fixnump raw-clpsize) raw-clpsize most-positive-fixnum))
	     (string (make-string clpsize)))
        (sys:far-peek string clppdata 0 clpsize)
        (win:GlobalUnlock clphdata)
        (acl:shorten-vector string (ct:strlen string))
        (with-input-from-string (input string)
          (get-stream-terms input))))))

