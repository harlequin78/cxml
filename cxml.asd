;;; XXX Die vielen verschiedenen Systeme hier sollten vielleicht
;;; Module eines grossen Systems CXML werden?

(defpackage :cxml-system
  (:use :asdf :cl))
(in-package :cxml-system)

(defclass closure-source-file (cl-source-file) ())

#+sbcl
(defmethod perform :around ((o compile-op) (s closure-source-file))
  ;; shut up already.  Correctness first.
  (handler-bind ((sb-ext:compiler-note #'muffle-warning))
    (let (#+sbcl (*compile-print* nil))
      (call-next-method))))

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

(defsystem runes
    :default-component-class closure-source-file
    :pathname (merge-pathnames
               "runes/"
               (make-pathname :name nil :type nil :defaults *load-truename*))
    :components
    ((:file "package")
     (:file dependent
	    :pathname
	    #+CLISP                             "dep-clisp"
	    #+(AND :CMU (NOT :PTHREAD))         "dep-cmucl"
	    #+sbcl                              "dep-sbcl"
	    #+(AND :CMU :PTHREAD)               "dep-cmucl-dtc"
	    #+(and allegro-version>= (version>= 5.0)) "dep-acl5"
	    #+(and allegro-version>= (not (version>= 5.0))) "dep-acl"
	    #-(or sbcl CLISP CMU allegro) #.(error "Configure!")
            :depends-on ("package"))
     (:file runes
            :pathname
             #-rune-is-character "runes"
             #+rune-is-character "characters"
	    :depends-on ("package" dependent))
     (:file "syntax" :depends-on ("package" dependent runes))
     (:file "util" :depends-on ("package" dependent))
     (:file "encodings" :depends-on ("package"))
     (:file "encodings-data" :depends-on ("package" "encodings"))
     (:file "xstream"
            :depends-on ("package" dependent "syntax" "encodings-data"))))

(asdf:defsystem :xml
    :default-component-class closure-source-file
    :pathname (merge-pathnames
               "xml/"
               (make-pathname :name nil :type nil :defaults *load-truename*))
    :components
    ((:file "package")
     (:file "sax-handler")
     (:file "characters"      :depends-on ("package"))
     (:file "xml-name-rune-p" :depends-on ("package"))
     (:file "split-sequence"  :depends-on ("package"))
     (:file "xml-parse"       :depends-on ("package" "sax-handler" "split-sequence"))
     (:file "characters"      :depends-on ("package")))
    :depends-on (:runes))

(asdf:defsystem :dom
    :default-component-class closure-source-file
    :pathname (merge-pathnames
               "dom/"
               (make-pathname :name nil :type nil :defaults *load-truename*))
    :components
    ((:file "package")
     (:file "dom-impl"        :depends-on ("package"))
     (:file "dom-builder"     :depends-on ("dom-impl"))
     (:file "xml-canonic"     :depends-on ("package"))
     (:file "simple-dom"      :depends-on ("package"))
     (:file "dom-sax"         :depends-on ("package")))
    :depends-on (:xml))

(asdf:defsystem :cxml-test
    :default-component-class closure-source-file
    :pathname (merge-pathnames
               "test/"
               (make-pathname :name nil :type nil :defaults *load-truename*))
    :components ((:file "domtest") (:file "xmlconf"))
    :depends-on (:xml :dom))

(asdf:defsystem :cxml :components () :depends-on (:dom :cxml-test))
