;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: GLISP-TEMP; -*-
;;; ---------------------------------------------------------------------------
;;;     Title: Generating a sane DEFPACKAGE for GLISP
;;;   Created: 1999-05-25
;;;    Author: Gilbert Baumann <unk6@rz.uni-karlsruhe.de>
;;; ---------------------------------------------------------------------------
;;;  (c) copyright 1999,2000 by Gilbert Baumann

(in-package :cl-user)

(defpackage :glisp
  (:use :cl)
  (:export #:defsubst

           ;; util.lisp :
;;;           #:always
;;;           #:cl-byte-stream
;;;           #:cl-char-stream
;;;           #:cl-stream
           #:compose
           #:curry
;;;           #:false
;;;           #:force
;;;           #:g/close
;;;           #:g/finish-output
;;;           #:g/peek-char
;;;           #:g/read-byte
;;;           #:g/read-byte-sequence
;;;           #:g/read-char
;;;           #:g/read-char-sequence
;;;           #:g/read-line
;;;           #:g/read-line*
;;;           #:g/unread-byte
;;;           #:g/unread-char
;;;           #:g/write-byte
;;;           #:g/write-byte-sequence
;;;           #:g/write-char
;;;           #:g/write-string
;;;           #:gstream
;;;           #:map-array
;;;           #:mapfcar
;;;           #:max*
;;;           #:maxf
;;;           #:min*
;;;           #:minf
;;;           #:multiple-value-or
;;;           #:multiple-value-some
;;;           #:nconcf
;;;           #:neq
;;;           #:promise
           #:rcurry
;;;           #:sanify-string
;;;           #:show
;;;           #:split-by
;;;           #:split-by-if
;;;           #:split-by-member
;;;           #:split-string
;;;           #:string-begin-equal
;;;           #:true
           #:until
;;;           #:use-byte-for-char-stream-flavour
;;;           #:use-char-for-byte-stream-flavour
           #:while
;;;           #:white-space-p
;;;
;;;           #:cl-byte-stream->gstream
;;;           #:cl-char-stream->gstream
;;;           #:g/open-inet-socket
;;;           #:accept-connection
;;;
;;;           #:find-temporary-file
;;;           #:delete-temporary-file
;;;           #:with-temporary-file
;;;          
;;;           #:set-equal
;;;           #:maybe-parse-integer
;;;           #:nop
;;;           #:with-structure-slots
;;;
;;;           #:compile-funcall
;;;           #:funcall*
;;;           #:mapc*
;;;           #:vreduce*
;;;           #:lreduce*
;;;           #:with-unique-names
    
           ;; runes.lisp
           #:rune
           #:rod
           #:simple-rod
           #:%rune
           #:rod-capitalize
           #:code-rune
           #:rune-code
           #:rune-downcase
           #:rune-upcase
           #:rod-downcase
           #:rod-upcase
           #:white-space-rune-p
           #:digit-rune-p
           #:rune=
           #:rune<=
           #:rune>=
           #:rune-equal
           #:runep
           #:sloopy-rod-p
           #:rod=
           #:rod-equal
           #:make-rod
           #:char-rune
           #:rune-char
           #:rod-string
           #:string-rod
           #:rod-subseq))
