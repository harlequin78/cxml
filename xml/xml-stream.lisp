;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: XML; readtable: glisp; Encoding: utf-8; -*-
;;; ---------------------------------------------------------------------------
;;;     Title: Fast streams
;;;   Created: 1999-07-17
;;;    Author: Gilbert Baumann <unk6@rz.uni-karlsruhe.de>
;;;   License: LGPL (See file COPYING for details).
;;; ---------------------------------------------------------------------------
;;;  © copyright 1999 by Gilbert Baumann

;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Library General Public License for more details.
;;;
;;; You should have received a copy of the GNU Library General Public
;;; License along with this library; if not, write to the 
;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330, 
;;; Boston, MA  02111-1307  USA.

(in-package :xml)

;;; API
;; 
;; MAKE-XSTREAM cl-stream &key name speed initial-speed         [function]
;; MAKE-ROD-XSTREAM rod &key name                               [function]
;; CLOSE-XSTREAM xstream                                        [function]
;; READ-RUNE xstream                                               [macro]
;; PEEK-RUNE xstream                                               [macro]
;; FREAD-RUNE xstream                                           [function]
;; FPEEK-RUNE xstream                                           [function]
;; XSTREAM-POSITION xstream                                     [function]
;; XSTREAM-LINE-NUMBER xstream                                  [function]
;; XSTREAM-COLUMN-NUMBER xstream                                [function]
;; XSTREAM-PLIST xstream                                        [accessor]
;; XSTREAM-ENCODING xstream                                     [accessor]  <-- be careful here. [*]

;; [*] swichting the encoding on the fly is only possible when the
;; stream's buffer is empty; therefore to be able to switch the
;; encoding, while some runes are already read, set the stream's speed
;; to 1 initially (via the initial-speed argument for MAKE-XSTREAM)
;; and later set it to full speed. (The encoding of the runes
;; sequence, you fetch off with READ-RUNE is always UTF-16 though).

;; An encoding is simply something, which provides the DECODE-SEQUENCE
;; method.

;;; Controller protocol
;;
;; READ-OCTECTS sequence os-stream start end -> first-non-written
;; XSTREAM/CLOSE os-stream
;;

(eval-when (eval compile load)
  (defparameter *fast* '(optimize (speed 3) (safety 0)))
  ;;(defparameter *fast* '(optimize (speed 2) (safety 3)))
  )

;; Let us first define fast fixnum arithmetric get rid of type
;; checks. (After all we know what we do here).

(defmacro fx-op (op &rest xs) 
  `(the fixnum (,op ,@(mapcar (lambda (x) `(the fixnum ,x)) xs))))
(defmacro fx-pred (op &rest xs) 
  `(,op ,@(mapcar (lambda (x) `(the fixnum ,x)) xs)))

(defmacro %+   (&rest xs) `(fx-op + ,@xs))
(defmacro %-   (&rest xs) `(fx-op - ,@xs))
(defmacro %*   (&rest xs) `(fx-op * ,@xs))
(defmacro %/   (&rest xs) `(fx-op floor ,@xs))
(defmacro %and (&rest xs) `(fx-op logand ,@xs))
(defmacro %ior (&rest xs) `(fx-op logior ,@xs))
(defmacro %xor (&rest xs) `(fx-op logxor ,@xs))
(defmacro %ash (&rest xs) `(fx-op ash ,@xs))
(defmacro %mod (&rest xs) `(fx-op mod ,@xs))

(defmacro %=  (&rest xs)  `(fx-pred = ,@xs))
(defmacro %<= (&rest xs)  `(fx-pred <= ,@xs))
(defmacro %>= (&rest xs)  `(fx-pred >= ,@xs))
(defmacro %<  (&rest xs)  `(fx-pred < ,@xs))
(defmacro %>  (&rest xs)  `(fx-pred > ,@xs))

(deftype buffer-index ()
  `(unsigned-byte ,(integer-length array-total-size-limit)))

(deftype buffer-byte ()
  `(unsigned-byte 16))

(deftype octet ()
  `(unsigned-byte 8))

;; The usage of a special marker for EOF is experimental and
;; considered unhygenic.

(defconstant +end+ #xFFFF
  "Special marker inserted into stream buffers to indicate end of buffered data.")

(defvar +null-buffer+ (make-array 0 :element-type 'buffer-byte))
(defvar +null-octet-buffer+ (make-array 0 :element-type 'octet))

(defstruct (xstream 
            (:constructor make-xstream/low)
            (:copier nil)
            (:print-function print-xstream))
  
  ;;; Read buffer
  
  ;; the buffer itself
  (buffer +null-buffer+ 
          :type (simple-array buffer-byte (*)))
  ;; points to the next element of `buffer' containing the next rune
  ;; about to be read.
  (read-ptr      0 :type buffer-index)
  ;; points to the first element of `buffer' not containing a rune to
  ;; be read.
  (fill-ptr      0 :type buffer-index)

  ;;; OS buffer
  
  ;; a scratch pad for READ-SEQUENCE
  (os-buffer +null-octet-buffer+
             :type (simple-array octet (*)))
  
  ;; `os-left-start', `os-left-end' designate a region of os-buffer,
  ;; which still contains some undecoded data. This is needed because
  ;; of the DECODE-SEQUENCE protocol
  (os-left-start 0 :type buffer-index)
  (os-left-end   0 :type buffer-index)
  
  ;; How much to read each time
  (speed         0 :type buffer-index)
  
  ;; Some stream object obeying to a certain protcol
  os-stream

  ;; The external format 
  ;; (some object offering the ENCODING protocol)
  (encoding :utf-8)

  ;;A STREAM-NAME object
  (name nil)
  
  ;; a plist a struct keeps the hack away
  (plist nil)
  
  ;; Stream Position
  (line-number 1 :type integer)         ;current line number
  (line-start  0 :type integer)         ;stream position the current line starts at
  (buffer-start 0 :type integer)        ;stream position the current buffer starts at
  
  ;; There is no need to maintain a column counter for each character
  ;; read, since we can easily compute it from `line-start' and
  ;; `buffer-start'.
  )

(defmacro read-rune (input)
  "Read a single rune off the xstream `input'. In case of end of file :EOF 
   is returned."
  `((lambda (input)
      (declare (type xstream input)
               #.*fast*)
      (let ((rp (xstream-read-ptr input)))
        (declare (type buffer-index rp))
        (let ((ch (aref (the (simple-array buffer-byte (*)) (xstream-buffer input))
                        rp)))
          (declare (type buffer-byte ch))
          (setf (xstream-read-ptr input) (%+ rp 1))
          (cond ((%= ch +end+)
                 (the (or (member :eof) rune)
                   (xstream-underflow input)))
                ((%= ch #x000A)         ;line break
                 (account-for-line-break input)
                 (code-rune ch))
                (t
                 (code-rune ch))))))
    ,input))

(defmacro peek-rune (input)
  "Peek a single rune off the xstream `input'. In case of end of file :EOF
   is returned."
  `((lambda (input)
      (declare (type xstream input)
               #.*fast*)
      (let ((rp (xstream-read-ptr input)))
        (declare (type buffer-index rp))
        (let ((ch (aref (the (simple-array buffer-byte (*)) (xstream-buffer input))
                        rp)))
          (declare (type buffer-byte ch))
          (cond ((%= ch +end+)
                 (prog1
                     (the (or (member :eof) rune) (xstream-underflow input))
                   (setf (xstream-read-ptr input) 0)))
                (t
                 (code-rune ch))))))
    ,input))

(defmacro consume-rune (input)
  "Like READ-RUNE, but does not actually return the read rune."
  `((lambda (input)
      (declare (type xstream input)
               #.*fast*)
      (let ((rp (xstream-read-ptr input)))
        (declare (type buffer-index rp))
        (let ((ch (aref (the (simple-array buffer-byte (*)) (xstream-buffer input))
                        rp)))
          (declare (type buffer-byte ch))
          (setf (xstream-read-ptr input) (%+ rp 1))
          (when (%= ch +end+)
            (xstream-underflow input))
          (when (%= ch #x000A)         ;line break
            (account-for-line-break input) )))
      nil)
    ,input))

(defsubst unread-rune (rune input)
  "Unread the last recently read rune; if there wasn't such a rune, you
   deserve to loose."
  (declare (ignore rune))
  (decf (xstream-read-ptr input))
  (when (%= (peek-rune input) #x000A)   ;was it a line break?
    (unaccount-for-line-break input)))

(defun fread-rune (input)
  (read-rune input))

(defun fpeek-rune (input)
  (peek-rune input))

;;; Line counting

(defun account-for-line-break (input)
  (declare (type xstream input))
  (incf (xstream-line-number input))
  (setf (xstream-line-start input)
    (+ (xstream-buffer-start input) (xstream-read-ptr input))))

(defun unaccount-for-line-break (input)
  ;; incomplete! 
  ;; We better use a traditional lookahead technique or forbid unread-rune.
  (decf (xstream-line-number input)))

;; User API:

(defun xstream-position (input)
  (+ (xstream-buffer-start input) (xstream-read-ptr input)))

;; xstream-line-number is structure accessor

(defun xstream-column-number (input)
  (+ (- (xstream-position input)
        (xstream-line-start input))
     1))

;;; Underflow

;;(defun read-runes (sequence input))

(defun xstream-underflow (input)
  (declare (type xstream input))
  ;; we are about to fill new data into the buffer, so we need to
  ;; adjust buffer-start.
  (incf (xstream-buffer-start input)
        (- (xstream-fill-ptr input) 0))
  (let (n m)
    ;; when there is something left in the os-buffer, we move it to
    ;; the start of the buffer.
    (setf m (- (xstream-os-left-end input) (xstream-os-left-start input)))
    (unless (zerop m)
      (replace (xstream-os-buffer input) (xstream-os-buffer input)
               :start1 0 :end1 m
               :start2 (xstream-os-left-start input)
               :end2 (xstream-os-left-end input))
      ;; then we take care that the buffer is large enough to carry at
      ;; least 100 bytes (a random number)
      (unless (>= (length (xstream-os-buffer input)) 100)
        (error "You lost")
        ;; todo: enlarge buffer
        ))
    (setf n
      (read-octets (xstream-os-buffer input) (xstream-os-stream input)
                   m (min (1- (length (xstream-os-buffer input)))
                          (+ m (xstream-speed input)))))
    (cond ((%= n 0)
           (setf (xstream-read-ptr input) 0
                 (xstream-fill-ptr input) n)
           (setf (aref (xstream-buffer input) (xstream-fill-ptr input)) +end+)
           :eof)
          (t
           (multiple-value-bind (fnw fnr) 
               (decode-sequence (xstream-encoding input) 
                                (xstream-os-buffer input) 0 n
                                (xstream-buffer input) 0 (1- (length (xstream-buffer input)))
                                (= n m))
             (setf (xstream-os-left-start input) fnr
                   (xstream-os-left-end input) n
                   (xstream-read-ptr input) 0
                   (xstream-fill-ptr input) fnw)
             (setf (aref (xstream-buffer input) (xstream-fill-ptr input)) +end+)
             (read-rune input))))))

;;; constructor

(defun make-xstream (os-stream &key name (speed 8192) (initial-speed 1))
  ;; XXX if initial-speed isn't 1, encoding will me munged up
  (assert (eql initial-speed 1))
  (let ()
    (multiple-value-bind (encoding preread) (figure-encoding os-stream)
      (let ((osbuf (make-array speed :element-type '(unsigned-byte 8))))
        (replace osbuf preread)
        (make-xstream/low
         :buffer (let ((r (make-array speed :element-type 'buffer-byte)))
                   (setf (%rune r 0) #xFFFF)
                   r)
         :read-ptr 0
         :fill-ptr 0
         :os-buffer osbuf
         :speed initial-speed
         :os-stream os-stream
         :os-left-start 0
         :os-left-end (length preread)
         :encoding encoding
         :name name)))))

(defmethod figure-encoding ((stream glisp:gstream))
  ;; For HTML iso-8859-1 is the default
  (values (xml::find-encoding :iso-8859-1) nil))

(defun make-rod-xstream (string &key name)
  ;; XXX encoding is mis-handled by this kind of stream
  (let ((n (length string)))
    (let ((buffer (make-array (1+ n) :element-type 'buffer-byte)))
      (declare (type (simple-array buffer-byte (*)) buffer))
      ;; copy the rod
      (do ((i (1- n) (- i 1)))
          ((< i 0))
        (declare (type fixnum i))
        (setf (aref buffer i) (rune-code (%rune string i))))
      (setf (aref buffer n) +end+)
      ;;
      (make-xstream/low :buffer buffer
                        :read-ptr 0
                        :fill-ptr n
                        ;; :os-buffer nil
                        :speed 1
                        :os-stream nil
                        :name name))))

;;; misc

(defun close-xstream (input)
  (xstream/close (xstream-os-stream input)))

;;; controller implementations

(defmethod read-octets (sequence (stream stream) start end)
  (#+CLISP lisp:read-byte-sequence
   #-CLISP read-sequence
           sequence stream :start start :end end))

(defmethod read-octets (sequence (stream null) start end)
  (declare (ignore sequence start end))
  0)

(defmethod xstream/close ((stream stream))
  (close stream))

(defmethod xstream/close ((stream null))
  nil)

