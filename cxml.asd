(defpackage :cxml-system
  (:use :asdf :cl))
(in-package :cxml-system)

(defclass closure-source-file (cl-source-file) ())

#+sbcl
(defmethod perform :around ((o compile-op) (s closure-source-file))
  ;; shut up already.  Correctness first.
  (handler-bind ((sb-ext:compiler-note #'muffle-warning))
    (call-next-method)))

(unless (find-package :glisp)
  (defpackage :glisp))

#-rune-is-character
(format t "~&;;; Building cxml with (UNSIGNED-BYTE 16) RUNES~%")

#+rune-is-character
(cond
  ((or #+(and allegro (not ics)) t)
    (error "Sorry, you cannot use RUNE-IS-CHARACTER in an 8 bit image."))
  ((or #+(or (and allegro ics) lispworks) t)
    (format t "~&;;; Building cxml with CHARACTER RUNES~%"))
  (t
    (error "CXML was configured to use character runes, but it is not known ~
            whether this Lisp has 16-bit characters")))

(defsystem glisp
    :default-component-class closure-source-file
    :pathname (merge-pathnames
               "glisp/"
               (make-pathname :name nil :type nil :defaults *load-truename*))
    :components
    ((:file dependent
	    :pathname
	    #+CLISP                             "dep-clisp"
	    #+(AND :CMU (NOT :PTHREAD))         "dep-cmucl"
	    #+sbcl                              "dep-sbcl"
	    #+(AND :CMU :PTHREAD)               "dep-cmucl-dtc"
	    #+(and allegro-version>= (version>= 5.0)) "dep-acl5"
	    #-(and allegro-version>= (version>= 5.0)) "dep-acl"
	    #-(or sbcl CLISP CMU allegro) #.(error "Configure!"))
     (:file "package"
	    :depends-on (dependent))
     (:file runes
            :pathname
             #-rune-is-character "runes"
             #+rune-is-character "characters"
	    :depends-on ("package" dependent))
     (:file "syntax"
	    :depends-on ("package" dependent runes))
     (:file "util"
	    :depends-on ("package" dependent))))

(asdf:defsystem :cxml
    :default-component-class closure-source-file
    :pathname (merge-pathnames
               "xml/"
               (make-pathname :name nil :type nil :defaults *load-truename*))
    :components
    ((:file "package"         :depends-on ("dompack"))
     (:file "encodings"       :depends-on ("package"))
     (:file "encodings-data"  :depends-on ("package" "encodings"))
     (:file "sax-handler")
     (:file "dompack")
     (:file "characters"      :depends-on ("package"))
     (:file "dom-impl"        :depends-on ("package" "dompack" "characters"))
     (:file "dom-builder"     :depends-on ("package" "dom-impl" "sax-handler"))
     (:file "xml-stream"      :depends-on ("package"))
     (:file "xml-name-rune-p" :depends-on ("package"))
     (:file "xml-parse"       :depends-on ("package" "dom-impl" "sax-handler" "encodings" "xml-stream"))
     (:file "xml-canonic"     :depends-on ("package" "dompack" "xml-parse"))
     #+(and allegro ics)
     (:file "string-dom"      :depends-on ("dom-impl")))
    :depends-on (:cl-package-aliases :glisp))
