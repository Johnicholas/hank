;;; -*- Mode: Lisp; Package: COMMON-LISP-USER -*-
;;;
;;; Author: Stuart Watt
;;;         Open University
;;;
;;; Design: Paul Mulholland
;;;         Stuart Watt
;;;         Open University
;;;
;;; A load file for HANK. Source code portability is a goal here, so
;;; that must be the excuse for the rather strange format of stuff
;;; in this file. We're trying to keep the system as portable as
;;; possible between Macs and PCs. 

(unless (member 'initialise-features acl::*system-init-fns*)
  (setf acl::*system-init-fns* (nconc acl::*system-init-fns* 
                                      '(initialise-features))))

(defun initialise-features ()
  (multiple-value-bind (win32p ignore major minor) (pc:win32p)
    (when (and win32p (pro:i>= major 4))
      (pushnew :hank-win95-look *features*))))

(initialise-features)

(let (#+Procyon-Common-Lisp (top::*warn-on-redefinition* nil)
      #+ACLPC (acl:*warn-on-protected-function-redefinition* nil))
  #+Runtime-System (load "c:\\lisp\\allegro\\fsl\\loadpix.fsl")
  #+ACLPC (load-system "hank" :name "ndlgfile")
  (load-system "hank" :name "cmu_loop")
  (load-system "hank" :name "packages")
  (load-system "hank" :name "port")
  #+ACLPC (load-system "hank" :name "dialogs"))

(loop for file in '(;; Low level guff
                    "globals" "macros" "widgets" "buttons" "predefs" 
                    "main" "auxiliar" "colours" 
                    "textures" "cursors"
                    "commands" "canvas" "drag" 
                    "graphs1" "clipbord" "graphs2" 
                    "segment" "graphs3" "control"
                    "format" "debugger"
                    
                    ;; Interface itself
                    "top" "grids" "tables1" "parts" "tokenise" 
                    "editor" "files" "tables2" "tables3" "tables4"
                    "moretop" "epsf" "export" "print"
                    
                    ;; Tracer/interpreter files come more or less at the end
                    "match" "ask" "execute"
                    "tracer" "events"
                    "prims" "rules"
                    )
      do (load-system "hank" :name file))
   
(defun build ()
  (in-package "HANK")

  (allegro::create-standalone-application
    (intern "RUNTIME-TOP-WITH-DEBUGGER" "HANK")
    :image "c:\\stuart\\hank\\hank.img"
    :package-directives '(("KEYWORD" . :keep)
                          ("PUBLIC" . :keep)
;                          ("COMMON-LISP" . :keep)
                          ("COMMON-LISP-USER" . :keep)
;                          ("COMMON-GRAPHICS" . :keep)
                          ("SYSTEM" . :keep)
;                          ("ALLEGRO" . :keep)
;                          ("WINDOWS" . :keep)
;                          ("PC" . :keep)
;                          ("C-TYPES" . :keep)
;                          ("DEBUGGER" . :keep)
;                          ("TEXT-EDIT" . :keep)
                          ("HANK-PRIMITIVES" . :keep)
                          ("HANK" . :keep)
                          )))