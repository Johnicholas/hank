;; Common File Dialog

;; Changelog
;; <1> rfr 12/93
;;     Started changelog
;;     Some openfilename cstruct elements changed type
;;     NT doesn't use allocalias16
;; <2> rfr 2/18/94
;;     use fi-openfilename
;; <3> cheetham 3/94
;;     small fix for defaulting directory intuitively
;; <4> cheetham 3/94
;;     turn off *save-verbose* by default (which writes
;;     a string to the toploop when you save a file)
;;     now that we show the * in the title bar of unsaved
;;     windows
;; <5> cheetham 3/94
;;     if error-code isn't found in the table of error names,
;;     then just print the code itself rather than NIL
;; <8> cheetham 4/94
;;     new status bar messages
;; <9> cheetham 4/94
;;     make ask-user-for-pathname default to the correct directory
;;     if the user's "host" argument ends in a slash
;; <10> cheetham 4/94
;;     make :load, :open, etc. default to the directory of the
;;     current window even when that directory is the root directory;
;;     this fix (done this way) depends on change <9>
;; <13> cheetham 7/8/94
;;     in directory-and-pathname-of-selected-window, if the passed-in
;;     window was *lisp-frame-window*, change it to *lisp-main-window*
;;     so that the selected window of that (used to find the default
;;     directory) will be the appropriate text-edit-window.  For some
;;     reason selecting Compile from the File menu was passing in
;;     *lisp-frame-window*, while Load (selected from the menu rather
;;     by using the keyboard shortcut) passed in *lisp-main-window*
;;     and Save As passed in the lisp editor pane!
;; <16> cheetham 12/2/94
;;     add the share-aware-p option to ask-user-for-new/existing-pathname
;;     so that users can select files that are currently open by another
;;     (or the same) application
;; <19> cheetham 1/3/95
;;     make the default directory for a fasl file that is to
;;     be created by the file compiler to be the directory of the
;;     first source code file in the list.  There was an obsolete
;;     hack in the code that used to do this by setting the current
;;     dos directory, but now the directory was always defaulting to
;;     the lisp.exe startup directory
;; <22> cheetham 5/3/95 (change made a while back)
;;     error message when user tries to locate aclwin source whose
;;     directory has not been pointed to
;; <27> cheetham 7/10/95
;;     make spaced-string-to-list use new general utility
;; <29> mm 30-Jan-96: more general allowed-type for 5.0
;; <30> cheetham 3/27/96
;;     move the text-edit package code from here to new file ndlgfilx.lsp
;;     to make room and to use only a single package in each file so that
;;     we can always evaluate individual function definitions with c-D.
;;     Comment out the other in-packages in here and use prefixes in a
;;     few spots where needed to put this whole file in the PC package.
;; <31> cheetham 5/24/96
;;     add the new builder extensions .bil and .bml to the lists of
;;     filename templates for the common file dialogs

(in-package :pc)

;; removed by cac 24may96
#+ignore
(defun fix-namestring-case (x) 
  #+(and ACLMERGE UNIX) x
  #+(or (not ACLMERGE) (not UNIX)) (string-downcase x))

(defconstant *scratch-string-length* 256)
(defparameter *scratch-lisp-string*
    (make-string *scratch-string-length*))
(defparameter *scratch-c-string*
    (ccallocate (char #.*scratch-string-length*)))

(defun scratch-c-string-to-lisp-string ()
   (cref (char #.*scratch-string-length*)
    *scratch-c-string* nil *scratch-lisp-string*
    (string #.*scratch-string-length*))
   (acl::substring *scratch-lisp-string* 0
       (ct:strlen *scratch-c-string*)))

(defun lisp-string-to-scratch-c-string (lisp-string)
 (let ((length (min (length lisp-string)
                  (1- *scratch-string-length*))))
  (cset (char #.*scratch-string-length*) *scratch-c-string*
     nil lisp-string (string length)) ;; #.*scratch-string-length*
  (cset (char #.*scratch-string-length*) *scratch-c-string*
     ((fixnum (min length (1- *scratch-string-length*))))
     0)
  *scratch-c-string*
  ))

(defun make-filter-string (dotted-pair)
 (format nil "~a (~a)~a~a~a"
    (car dotted-pair)(cdr dotted-pair)(int-char 0)
    (cdr dotted-pair)(int-char 0)))

(defvar *edit-allowed-types*   ;; <29>
    '(#+ACLMERGE ("Lisp files" . "*.cl")
      ("Lisp files" . "*.lsp")
      #+ACLMERGE ("Lisp files" . "*.lisp")
      ("Builder files" . "*.bil;*.bml") ;; <31>
      ("All files" . "*.*")))

(defun ask-user-for-existing-pathname
    (prompt &key
       (host (namestring *default-pathname-defaults*))
       (stream (selected-window-or-screen))
       (allowed-types *edit-allowed-types*)   ;; <29>
       (initial-name "")
       (multiple-p nil)
       (change-current-directory-p t)
       (share-aware-p nil) ;; <16>
       )
   (ask-user-for-pathname prompt host stream
      allowed-types initial-name
      nil multiple-p change-current-directory-p nil
      share-aware-p)) ;; <16>

(defun ask-user-for-new-pathname
    (prompt &key
       (host (namestring *default-pathname-defaults*))
       (stream (selected-window-or-screen))
       (allowed-types *edit-allowed-types*)   ;; <29>
       (initial-name "")
       (multiple-p nil)
       (change-current-directory-p t)
       (warn-if-exists-p t)
       (share-aware-p nil) ;; <16>
       )
   (ask-user-for-pathname prompt host stream
      allowed-types initial-name
      t multiple-p change-current-directory-p
      warn-if-exists-p share-aware-p)) ;; <16>
3
(defun ask-user-for-pathname
    (prompt host stream allowed-types initial-name
     save-p multiple-p change-current-directory-p
     warn-if-exists-p share-aware-p) ;; <16>
  (let* ((open-file-struct (ccallocate openfilename))
	 #-ACLMERGE
	 (host-length (and host (length host))))
    (csets fi-openfilename open-file-struct ; <2>
	   struct-size (sizeof openfilename)
	   owner (window-handle stream)
	   hinst *hinst*
	   file-filter
	   (apply #'concatenate 'string
		  (mapcar #'make-filter-string allowed-types))
	   custom-filter (ccallocate (:void *) :initial-value 0)
	   max-custom-filter 0 ;; length of custom filter string

	   ;; zero means use custom-filter if supplied
	   ;; otherwise the first filter in the list
	   filter-index 0

	   selected-file (lisp-string-to-scratch-c-string
			  (or initial-name ""))
	   max-file *scratch-string-length*
	   file-title (ccallocate (:void *) :initial-value 0)
					; with no path
	   max-file-title 0
	   initial-dir (#||#
			#+ACLMERGE physical-namestring
			#-ACLMERGE identity
			(if host
			    #+ACLMERGE host
			    #-ACLMERGE
			    (if (and
				 ;; If last char is a slash
				 (eql #\\ (aref host (1- host-length)))
				 ;; but it's not the ONLY slash
				 (not (eq host-length
					  (1+ (position #\\ host)))))
				;; then remove the final slash
				(subseq host 0 (1- host-length)) ;; <9>
			      host)
			    (namestring (pathname
					 *default-pathname-defaults*))))
	   window-title prompt
	   flags (logior
		  (if multiple-p ofn_allowmultiselect 0)
		  (if save-p 0 ofn_filemustexist)
		  (if warn-if-exists-p ofn_overwriteprompt 0)
		  (if change-current-directory-p
		      0 ofn_nochangedir)
		  (if share-aware-p ofn_shareaware 0) ;; <16>
		  ofn_hidereadonly
		  )
	   default-extension (ccallocate (:void *) :initial-value 0)
	   custom-data 0 ;; would be passed to the callback
	   ;; callback ;; ignored since we don't pass that flag
	   ;; template-name ;; ignored
	   )
    (let ((error-code (if save-p
			  (GetSaveFileName open-file-struct)
			(GetOpenFileName open-file-struct))))
      
      (if error-code ;; t means it worked
	  (values                                                          ;; SNW
            (if multiple-p
	        (pathnames-from-directory-and-filenames
	          (spaced-string-to-list
		    (scratch-c-string-to-lisp-string)))
	        (pathname			; added by cac, 5-apr-94
	          (scratch-c-string-to-lisp-string)))
            (1- (cref fi-openfilename open-file-struct (filter-index))))   ;; SNW
	(let ((error-code (CommDlgExtendedError)))
	  (and (plusp error-code) ;; zero means cancelled, so return NIL
	       (error (format nil 
			      "Common dialog error ~a."
			      (or (cdr (assoc error-code
					      common-dialog-errors))
				  error-code))))))))) ;; <5>

(defun pathnames-from-directory-and-filenames (filename-list)
   ;; Takes a list consisting of a directory namestring followed
   ;; by a set of filenames relative to that directory.
   ;; This is the sort of list returned by the common dialog
   ;; when multiple choices are allowed.
   ;; Returns a list of complete pathnames.
   (if (eq (length filename-list) 1)
      ;; If only one choice, no separate directory is returned
      ;; by the GetOpenFileName call.
      filename-list
      (let ((directory (car filename-list)))
         ;; Windows doesn't stick a backslash on the end of the dir.
         #+(or (not ACLMERGE) (and 386 (not UNIX)))
         (unless (eql (aref directory (1- (length directory)))
                    #\\)
            (setf directory (concatenate 'string directory "\\")))
         (mapcar #'(lambda (filename)
                      ;; cac removed call to namestring to have this function
                      ;; return pathnames instead of strings.
                      ;; 5-apr-94
                      (merge-pathnames filename directory))
            (cdr filename-list)))))

(defun spaced-string-to-list (string) ;; <27>
   (delimited-string-to-list string #\space))

;; -----------------------------------------------------------
;; This section changes old functionality to call
;; the new common dialog functions ask-user-for-new/existing-pathname

#+old ;; <30>
(in-package :cg)

(defun directory-and-pathname-of-selected-window
    (&optional (window *lisp-main-window*))
   ;; This new utility function finds the pathname associated
   ;; with the currently selected window, when the selected window
   ;; is editing a file.  Useful for selecting files in the same
   ;; directory as the file currently being worked on.  (Cheetham 8/93)
   (when (eq window *lisp-frame-window*) ;; <13>
      (setq window *lisp-main-window*)) ;; <13>
   (let* ((current-window (or (selected-window window)
                              window)) ;; <3>
          (path (and (or (typep current-window 'text-edit-window)
                         (typep current-window 'text-edit-pane))
                     (te:file current-window)))
          #-ACLMERGE
          (path-string (and path (namestring path)))
          #-ACLMERGE
          (final-slash-pos (position #\\ path-string :from-end t))
          (directory (and #-ACLMERGE path-string #+ACLMERGE path
                          #+ACLMERGE
                          (pathname-directory-pathname path)
                          #-ACLMERGE
                          (subseq path-string 0
                             (and final-slash-pos
                                  (1+ final-slash-pos)))))) ;; <10>
      ;; Return the directory string and the whole pathname object.
      (values directory path)))

#+old ;; <30>
(in-package :allegro)

#-runtime-system
(defmethod :compile ((window basic-pane))
   (multiple-value-bind (default-directory default-path)
       (directory-and-pathname-of-selected-window window)
      (let* ((title "Compile File")
             (input-files (ask-user-for-existing-pathname
                           "File(s) to Compile"
                           :stream *lisp-main-window*
                           :multiple-p t
                           :host default-directory
                           :initial-name (and default-path
                                              (file-namestring default-path))
                           ))
             (output-file 
              (and input-files 
                   (ask-user-for-new-pathname
                    "FSL File to Create"
                    :initial-name (file-namestring 
                                     (merge-pathnames
                                        ".fsl"
                                        (first input-files)))
                    :allowed-types '(("FSL Files" . "*.fsl")
                                     ("All Files" . "*.*"))
                    :host (directory-namestring
                             (first input-files)) ;; <19>
                    ))))
         (when output-file 
            (lisp-message "Compiling the files ~(~s~) to ~(~s~) ..."
               (mapcar #'namestring input-files)
               (namestring output-file)) ;; <8>
            (toploop:top-eval 
             `(compile-file ',input-files :output-file ',output-file))))))

#+old ;; <30>
(in-package :toploop)

(defvar top::*load-allowed-types*    ;; <29>
    '(#+ACLMERGE ("Lisp files" . "*.cl;*.lsp;*.fasl")
      #+ACLPC("Lisp Files" . "*.lsp;*.fsl")
      ("LSP Files" . "*.lsp")
      #+ACLPC ("FSL Files" . "*.fsl")
      #+ACLMERGE ("Fasl Files" . "*.fasl")
      ("Builder files" . "*.bil;*.bml") ;; <31>
      ("All Files" . "*.*")))

(defmethod :load ((window basic-pane))
   (multiple-value-bind (default-directory default-path)
       (directory-and-pathname-of-selected-window window)
      (let* ((files-to-load 
              (ask-user-for-existing-pathname
               "File(s) to Load"
               :stream *lisp-main-window*
               :multiple-p t
               :allowed-types top::*load-allowed-types*   ;; <29><30>
               ;; By default, offer to load the file currently
               ;; displayed in the selected text-edit window.
               :host default-directory
               :initial-name (and default-path
                                  (file-namestring default-path))
               )))
         (when files-to-load 
            (dolist (file-to-load files-to-load)
               #-runtime-system
               (lisp-message "Loading file ~s ..." file-to-load) ;; <8>
               (top:top-eval `(load ',file-to-load))) ;; <30>
            #-runtime-system
            (if (eq (length files-to-load) 1)
               (lisp-message "LOADED file ~s" ;; <8>
                  (car files-to-load))
               (lisp-message "LOADED ~(~r~) files"
                  (length files-to-load)))
            (beep window)))))

;; from inspect/pnameins.lsp
#+old ;; #-runtime-system <30>
(in-package :inspector)

#-runtime-system
(defun inspector::ask-for-new-name (prompt pathname)
   (let ((path (namestring pathname)))
      (ask-user-for-existing-pathname (or prompt "Inspect File")
       :stream *lisp-main-window*
       :initial-name (file-namestring pathname)
       :host
       #+ACLMERGE (pathname-directory-pathname path)
       #-ACLMERGE (subseq path 0
                     (position #\\ path :from-end t)))))
