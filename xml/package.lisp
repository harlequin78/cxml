(in-package :cl-user)

(defpackage :encoding
  (:use :cl :glisp)
  (:export
   #:find-encoding
   #:decode-sequence))

(defpackage :xml
  (:use :cl :glisp :encoding)
  (:export
   ;; xstreams
   #:make-xstream
   #:make-rod-xstream
   #:close-xstream
   #:read-rune
   #:peek-rune
   #:unread-rune
   #:fread-rune
   #:fpeek-rune
   #:xstream-position
   #:xstream-line-number
   #:xstream-column-number
   #:xstream-plist
   #:xstream-encoding
   
   ;; xstream controller protocol
   #:read-octects
   #:xstream/close

   #:attribute-namespace-uri
   #:attribute-local-name
   #:attribute-qname
   #:attribute-value
   
   #:parse-file
   #:parse-stream
   ;; XXX encoding is mis-handled by parse-string, don't export it
   ;; #:parse-string

   #:unparse-document))
