(in-package :cl-user)

(defpackage :encoding
  (:use :glisp)
  (:export
   #:find-encoding
   #:decode-sequence))

(defpackage :xml
  (:use 
   :glisp
   :encoding)
  (:alias (:cdom :dom))
  
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
   #:parse-string

   #:unparse-document))
