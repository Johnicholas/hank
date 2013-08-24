;;; -*- Mode: Lisp; Package: POSTSCRIPT -*-
;;;
;;; Author: Stuart Watt
;;;         The Open University
;;;
;;; Design: Paul Mulholland
;;;         Stuart Watt
;;;         The Open University

(in-package "POSTSCRIPT" :nicknames '("PS"))

(export '(encapsulated-postscript-device) "POSTSCRIPT")

(defconstant common-graphics-prolog (#+ACLPC pc::cr-to-crlf 
                                     #+PRocyon-Common-Lisp identity
"48 dict begin
/WordBreak ( ) def
/FontCorrection -1.5 def
/cgshow
  { currentfont /FontBBox get aload pop currentfont /FontMatrix get transform FontCorrection add /dy exch def pop pop pop
    0 dy neg rmoveto show 0 dy rmoveto } def
/DrawBox {
  /top exch def
  /right exch def
  /bottom exch def
  /left exch def
  currentpoint currentdash currentrgbcolor [] 0 setdash 1 setgray
  newpath left bottom moveto left top lineto right top lineto right bottom lineto closepath stroke
  setrgbcolor setdash 
  newpath left bottom moveto left top lineto right top lineto right bottom lineto closepath stroke moveto } def
/DrawStringInRectangle {
  /vjustify exch def
  /hjustify exch def
  currentfont /FontBBox get aload pop exch pop exch sub currentfont /FontMatrix get transform
    /lineheight exch def pop
  /top exch def
  /right exch def
  /bottom exch def
  /left exch def
  /textstring exch def
  currentfont /FontBBox get aload pop currentfont /FontMatrix get transform /ascent exch def pop pop pop
  /width right left sub def
  /height top bottom sub def

  /BreakIntoLines {
    /proc exch def
    /linewidth exch def
    /textstring exch def
    /breakwidth WordBreak stringwidth pop def
    /currentwidth 0 def
    /lastwordbreak 0 def
    /startchar 0 def
    /restoftext textstring def
    { restoftext WordBreak search
      { /nextword exch def pop
        /restoftext exch def
        /wordwidth nextword stringwidth pop def
        currentwidth wordwidth add linewidth gt
        { textstring startchar lastwordbreak startchar sub getinterval 
          proc
          /startchar lastwordbreak def
          /currentwidth wordwidth breakwidth add def }
        { /currentwidth currentwidth wordwidth add
          breakwidth add def }
        ifelse
        /lastwordbreak lastwordbreak nextword length add 1 add def }
      { pop exit }
      ifelse }
    loop
    /lastchar textstring length def
    textstring startchar lastchar startchar sub getinterval proc } def
  /LineCount {
    /lines 0 def
    { pop /lines lines 1 add def } BreakIntoLines
    lines } def
  /StringTrim {
    /thestring exch def
    /textindex thestring length 1 sub def
    { thestring textindex get 32 ne { exit } if textindex 0 eq {exit} if
      /textindex textindex 1 sub def } loop
    thestring 0 textindex 1 add getinterval } def
  /textheight textstring width LineCount lineheight mul def
  /yposition height textheight sub vjustify mul top exch sub ascent sub FontCorrection sub def
  textstring width
  { StringTrim /linestring exch def
    /drawwidth linestring stringwidth pop def
    /dleft left width drawwidth sub hjustify mul add def
    /dtop yposition def
    currentrgbcolor currentpoint newpath
    dleft dtop moveto drawwidth 0 rlineto 0 lineheight rlineto drawwidth neg 0 rlineto 0 lineheight neg rlineto closepath
    1 setgray fill moveto setrgbcolor
    dleft dtop moveto linestring show
    /yposition yposition lineheight sub def }
  BreakIntoLines } def
/concatprocs
  { /proc2 exch cvlit def
    /proc1 exch cvlit def
    /newproc proc1 length proc2 length add array def
    newproc 0 proc1 putinterval
    newproc proc1 length proc2 putinterval
    newproc cvx } def
/graphimage
  { /py exch def
    /px exch def
    /picstr px 8 div ceiling cvi string def
    gsave
    translate scale
    { 1 exch sub } currenttransfer concatprocs settransfer
    px py 1 [px 0 0 py neg 0 py]
    { currentfile picstr readhexstring pop } image
    grestore 
  } def
"))

(defclass encapsulated-postscript-device (cg:text)
  ((foreground-color
     :initform cg:black)
   (background-color
     :initform cg:white)
   (fill-texture
     :initform :foreground)
   (font
     :initform (cg:make-font nil nil 10))
   (fonts
     :initform nil)))

(defconstant eps-format-buffer (make-array 1024 :element-type 'character :fill-pointer t))
(defconstant eps-write-buffer (ct:ccallocate (char 1024)))

(defun eps-format (stream string &rest arguments)
  (setf (fill-pointer eps-format-buffer) 0)
  (apply #'cl:format eps-format-buffer string arguments)
  (let ((length (fill-pointer eps-format-buffer)))
    (replace eps-write-buffer eps-format-buffer :start2 0 :end2 length)
    (setf (char eps-write-buffer length) #\Return)
    (setf (char eps-write-buffer (+ 1 length)) #\Linefeed)
    (cg:location-write-string stream eps-write-buffer 0 (+ 2 length))))
    
(defmethod cg:device-open ((stream encapsulated-postscript-device) options)
  (call-next-method)
  (setf (cg:page-width stream t) (getf options :page-width 0))
  (setf (cg:page-height stream t) (getf options :page-height 0))
  (eps-format stream "%!PS-Adobe-3.0 EPSF-3.0")
  (eps-format stream "%%BoundingBox: ~D ~D ~D ~D" 
    (getf options :left-margin 0) (getf options :top-margin 0)
    (getf options :right-margin 0) (getf options :bottom-margin 0))
  (eps-format stream "%%Title: ~A" (or (cg:stream-title stream) "Untitled"))
  (eps-format stream "%%Creator: Common Graphics EPS 0.1")
  (eps-format stream "%%DocumentNeededResources: (atend)")
  (eps-format stream "%%EndComments")
  (eps-format stream "%%BeginProcSet: CommonLispGraphics_header 1 0")
  (cg:location-write-string stream common-graphics-prolog 0 (length common-graphics-prolog))
  (eps-format stream "%%EndProcSet")
  (eps-format stream "%%BeginSetup")
  (cg:set-font stream (cg:make-font nil :helvetica 13))
  (cg:move-to stream '#.(cg:make-position 0 0))
  (eps-format stream "0.5 setlinewidth")
  (when (getf options :font-correction)
    (eps-format stream "/FontCorrection ~A def" (getf options :font-correction)))
  (eps-format stream "%%EndSetup"))

(defmethod cg:stream-units-per-inch ((stream encapsulated-postscript-device))
  72)

(defmethod write-trailer ((stream encapsulated-postscript-device))
  (eps-format stream "%%Trailer")
  (loop with firstp = t
        for font in (slot-value stream 'fonts)
        do (eps-format stream (if firstp "%%DocumentNeededResources: font ~A" "%%+ font ~A") font)
           (setf firstp nil))
  (eps-format stream "end"))

(defmethod cg:device-close ((stream encapsulated-postscript-device) abort)
  (unless abort
    (write-trailer stream)
    (eps-format stream "%%EOF"))
  (call-next-method))

(defun npostscript-position (stream position)
  (setf (cg:position-y position) (- (cg:page-height stream t) (cg:position-y position)))
  position)

(defmethod cg:move-to ((stream encapsulated-postscript-device) position)
  (let ((position (npostscript-position stream (cg:ncopy-position '#.(cg:make-position 0 0) position))))
    (eps-format stream "~D ~D moveto" (cg:position-x position) (cg:position-y position))))

(defmethod cg:draw-to ((stream encapsulated-postscript-device) position)
  (let ((position (npostscript-position stream (cg:ncopy-position '#.(cg:make-position 0 0) position))))
    (eps-format stream "currentpoint newpath moveto ~D ~D lineto stroke" (cg:position-x position) (cg:position-y position))))

(defmethod cg:set-line-dashing ((stream encapsulated-postscript-device) dashing)
  (ecase dashing
    ((:solid)
     (eps-format stream "[] 0 setdash"))
    ((:dash)
     (eps-format stream "[12 3] 0 setdash"))))

(defmethod cg:foreground-color ((stream encapsulated-postscript-device) &optional (rgb (cg:make-rgb)))
  (slot-value stream 'foreground-color))

(defmethod cg:set-foreground-color ((stream encapsulated-postscript-device) colour)
  (setf (slot-value stream 'foreground-color)
          (if (cg:rgb-p colour)
              colour
              (setf colour (elt hank::*colour-vector* colour))))
  (eps-format stream "~D ~D ~D setrgbcolor"
    (/ (cg:rgb-red colour) 255.0) (/  (cg:rgb-green colour) 255.0) (/ (cg:rgb-blue colour) 255.0)))

(defmethod cg:set-background-color ((stream encapsulated-postscript-device) colour)
  (setf (slot-value stream 'background-color)
          (if (cg:rgb-p colour)
              colour
              (elt hank::*colour-vector* colour))))

(defmethod cg:paint-operation ((stream encapsulated-postscript-device))
  #+ACLPC cg:po-replace
  #+Procyon-Common-Lisp cg:replace)
(defmethod cg:set-paint-operation ((stream encapsulated-postscript-device) operation)
  nil)

(defmethod cg:fill-texture ((stream encapsulated-postscript-device))
  (slot-value stream 'fill-texture))

(defmethod cg:set-fill-texture ((stream encapsulated-postscript-device) texture)
  (setf (slot-value stream 'fill-texture) texture)
  (ecase texture
    ((nil))
    ((:solid :foreground)
     (let ((colour (slot-value stream 'foreground-color)))
       (eps-format stream "~D ~D ~D setrgbcolor"
         (/ (cg:rgb-red colour) 255.0) (/  (cg:rgb-green colour) 255.0) (/ (cg:rgb-blue colour) 255.0))))
    ((:background :blank)
     (let ((colour (slot-value stream 'background-color)))
       (eps-format stream "~D ~D ~D setrgbcolor"
         (/ (cg:rgb-red colour) 255.0) (/  (cg:rgb-green colour) 255.0) (/ (cg:rgb-blue colour) 255.0))))))
             
(defmethod cg:background-texture ((stream encapsulated-postscript-device))
  :background)

(defun convert-polygon-points (stream polygon)
  (let ((height (cg:page-height stream t)))
    (loop for position in polygon
          collect (cg:position-x position)
          collect (- height (cg:position-y position)))))

(defmethod cg:fill-polygon ((stream encapsulated-postscript-device) polygon)
  (eps-format stream "~1{currentpoint newpath ~D ~D moveto ~@{~D ~D lineto ~} closepath fill moveto~}"
    (convert-polygon-points stream polygon)))

(defmethod cg:fill-box ((stream encapsulated-postscript-device) box)
  (let* ((height (cg:page-height stream t))
         (left (cg:box-left box))
         (top (- height (cg:box-top box)))
         (right (cg:box-right box))
         (bottom (- height (cg:box-bottom box))))
    (eps-format stream "currentpoint newpath ~D ~D moveto ~D ~D lineto ~D ~D lineto ~D ~D lineto ~D ~D lineto closepath fill moveto"
      left top right top right bottom left bottom left top)
    ))

(defmethod cg:draw-polygon ((stream encapsulated-postscript-device) polygon)
  (eps-format stream "~1{currentpoint newpath ~D ~D moveto ~@{~D ~D lineto ~} closepath stroke moveto~}"
    (convert-polygon-points stream polygon)))

(defmethod cg:draw-polyline ((stream encapsulated-postscript-device) polygon)
  (eps-format stream "~1{currentpoint newpath ~D ~D moveto ~@{~D ~D lineto ~} stroke moveto~}"
    (convert-polygon-points stream polygon)))
   
(defmethod cg:draw-box ((stream encapsulated-postscript-device) box)
  (let* ((height (cg:page-height stream t))
         (left (cg:box-left box))
         (top (- height (cg:box-top box)))
         (right (cg:box-right box))
         (bottom (- height (cg:box-bottom box))))
    (eps-format stream "~D ~D ~D ~D DrawBox" left bottom right top)))
   
(defmethod cg:set-clipping-box ((stream encapsulated-postscript-device) box)
  ())
(defmethod cg:nclipping-box ((stream encapsulated-postscript-device) box)
  (cg:nmake-box box -32768 -32768 32767 32767))

(defmethod cg:copy-pixels-to-stream ((stream encapsulated-postscript-device) texture texture-info
                                     from-box to-box operation)
  ())

(defmethod cg:device-write-string ((stream encapsulated-postscript-device) string start end)
  (cg:location-write-string stream "(" 0 1)
  (let ((string (subseq string start end)))
    (cg:location-write-string stream string 0 (length string)))
  (cg:location-write-string stream #.(cl:format () ") cgshow~C~C" #\Return #\Linefeed) 0 10)
  )

(defmethod cg:set-line-width ((stream encapsulated-postscript-device) width)
  ())

(defun justify-proportion (justify)
  (ccase justify
    ((:left :bottom) 0.0)
    ((:centre :center) 0.5)
    ((:right :top) 1.0)))

(defconstant postscript-string-buffer-size 1024)

(defun postscript-string (string)
  (let ((buffer '#.(make-string postscript-string-buffer-size))
        (index 0)
        (code nil))
    (loop for character across string
          do (cond ((or (eql character #\() (eql character #\)) (eql character #\\))
                    (setf (schar buffer index) #\\)
                    (pro:iincf index))
                   ((pro:i< (setf code (cl:char-int character)) 32)
                    (setf (schar buffer index) #\\)
                    (pro:iincf index)
                    (setf (schar buffer index) (digit-char (ldb (byte 3 6) code)))
                    (pro:iincf index)
                    (setf (schar buffer index) (digit-char (ldb (byte 3 3) code)))
                    (pro:iincf index)
                    (setf (schar buffer index) (digit-char (ldb (byte 3 0) code)))
                    (setf character nil)))
             (when character
               (setf (schar buffer index) character)
               (pro:iincf index)))
    (subseq buffer 0 index)))

(defmethod cg:draw-string-in-box ((stream encapsulated-postscript-device) string start end box
                                  horizontal-justification vertical-justification
                                  . #+ACLPC (&optional string-vertical-p wrap-p)
                                    #+Procyon-Common-Lisp (string-vertical-p))
  (when (pro:i= start end)
    (return-from draw-string-in-box))
  (let ((hjustify (justify-proportion horizontal-justification))
        (vjustify (justify-proportion vertical-justification))
        (height (cg:page-height stream)))
    (eps-format stream "(~A) ~A ~A ~A ~A ~A ~A DrawStringInRectangle~%"
      (postscript-string (subseq string start end))
      (cg:box-left box) (- height (cg:box-bottom box))
      (cg:box-right box) (- height (cg:box-top box))
      hjustify vjustify)))

(defmethod cg:set-font ((stream encapsulated-postscript-device) font &optional set-line-height-too)
  (setf (slot-value stream 'font) font)
  (let ((face (cg:font-face font)))
    (setf face (get face :postscript-name face))
    (setf face (etypecase face
                 (keyword (string-capitalize (symbol-name face)))
                 (string face)))
    (setf face (format () "~A~@[-~@(~{~A~}~)~]" face (cg:font-style font)))
    (unless (find face (slot-value stream 'fonts) :test #'pro:whole-string=)
      (eps-format stream "%%IncludeResource: font ~A" face)
      (push face (slot-value stream 'fonts)))
    (eps-format stream "/~A findfont ~A scalefont setfont"
      face (cg:font-size font))))

(defmethod cg:font ((stream encapsulated-postscript-device))
  (slot-value stream 'font))
(defmethod cg:font-handle ((stream encapsulated-postscript-device))
  (slot-value stream 'font))

