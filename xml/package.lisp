(in-package :cl-user)

(defpackage :cxml
  (:use :cl :runes :encoding)
  (:import-from #+sbcl :sb-gray
                #+allegro :excl
                #+cmu :ext
                #+clisp :gray
                #-(or sbcl allegro cmu clisp) ...
                #:fundamental-binary-input-stream
                #:stream-read-sequence
                stream-read-byte)
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
   #:parse-octets

   #:unparse-document
   #:unparse-document-to-octets

   #:validity-error))
