;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: CXML; readtable: runes; Encoding: utf-8; -*-
;;; ---------------------------------------------------------------------------
;;;     Title: Dump canonic XML according to J.Clark
;;;   Created: 1999-09-09
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

(in-package :cxml)

;; 
;; | Canonical XML
;; | =============
;; |                                      
;; | This document defines a subset of XML called canonical XML. The
;; | intended use of canonical XML is in testing XML processors, as a
;; | representation of the result of parsing an XML document.
;; | 
;; | Every well-formed XML document has a unique structurally equivalent
;; | canonical XML document. Two structurally equivalent XML documents have
;; | a byte-for-byte identical canonical XML document. Canonicalizing an
;; | XML document requires only information that an XML processor is
;; | required to make available to an application.
;; | 
;; | A canonical XML document conforms to the following grammar:
;; |
;; |    CanonXML    ::= Pi* element Pi*
;; |    element     ::= Stag (Datachar | Pi | element)* Etag
;; |    Stag        ::= '<'  Name Atts '>'
;; |    Etag        ::= '</' Name '>'
;; |    Pi          ::= '<?' Name ' ' (((Char - S) Char*)? - (Char* '?>' Char*)) '?>'
;; |    Atts        ::= (' ' Name '=' '"' Datachar* '"')*
;; |    Datachar    ::= '&amp;' | '&lt;' | '&gt;' | '&quot;'
;; |                     | '&#9;'| '&#10;'| '&#13;'
;; |                     | (Char - ('&' | '<' | '>' | '"' | #x9 | #xA | #xD))
;; |    Name        ::= (see XML spec)
;; |    Char        ::= (see XML spec)
;; |    S           ::= (see XML spec)
;; |
;; | Attributes are in lexicographical order (in Unicode bit order).
;; |  
;; | A canonical XML document is encoded in UTF-8.
;; |  
;; | Ignorable white space is considered significant and is treated
;; | equivalently to data.
;;
;; -- James Clark (jjc@jclark.com)


;;;; SINK: a rune output "stream"

(defclass sink ()
    ((high-surrogate :initform nil)
     (column :initform 0 :accessor column)
     (width :initform 79 :initarg :width :accessor width)
     (canonical :initform t :initarg :canonical :accessor canonical)
     (indentation :initform nil :initarg :indentation :accessor indentation)
     (notations :initform (make-buffer :element-type t) :accessor notations)
     (name-for-dtd :accessor name-for-dtd)
     (previous-notation :initform nil :accessor previous-notation)
     (stack :initform nil :accessor stack)))

(defmethod initialize-instance :after ((instance sink) &key)
  (when (eq (canonical instance) t)
    (setf (canonical instance) 1))
  (unless (member (canonical instance) '(nil 1 2))
    (error "Invalid canonical form: ~A" (canonical instance)))
  (when (and (canonical instance) (indentation instance))
    (error "Cannot indent XML in canonical mode")))

(defclass vector-sink (sink)
    ((target-vector :initform (make-buffer))))

(defclass character-stream-sink (sink)
    ((target-stream :initarg :target-stream)))

;; WRITE-OCTET als generisch zu machen ist vielleicht nicht die schnellste
;; Loesung, aber die einfachste.
(defgeneric write-octet (octet sink))

(defun make-buffer (&key (element-type '(unsigned-byte 8)))
  (make-array 1
              :element-type element-type
              :adjustable t
              :fill-pointer 0))

(defmethod write-octet :after (octet sink)
  (with-slots (column) sink
    (setf column (if (eql octet 10) 0 (1+ column)))))


;; vector (octet) sinks

(defun make-octet-vector-sink (&rest initargs)
  (apply #'make-instance 'vector-sink initargs))

(defmethod write-octet (octet (sink vector-sink))
  (let ((target-vector (slot-value sink 'target-vector)))
    (vector-push-extend octet target-vector (length target-vector))))

(defmethod sax:end-document ((sink vector-sink))
  (slot-value sink 'target-vector))

(defun unparse-document (doc character-stream &rest initargs)
  (let ((sink (apply #'make-character-stream-sink character-stream initargs)))
    (dom:map-document sink doc :include-default-values t)))


;; character stream sinks

(defun make-character-stream-sink (character-stream &rest initargs)
  (apply #'make-instance 'character-stream-sink
         :target-stream character-stream
         initargs))

(defmethod write-octet (octet (sink character-stream-sink))
  (write-char (code-char octet) (slot-value sink 'target-stream)))

(defmethod sax:end-document ((sink character-stream-sink))
  (slot-value sink 'target-stream))

(defun unparse-document-to-octets (doc &rest initargs)
  (let ((sink (apply #'make-octet-sink initargs)))
    (dom:map-document sink doc :include-default-values t)))


;;;; doctype and notations

(defmethod sax:start-dtd ((sink sink) name public-id system-id)
  (declare (ignore public-id system-id))
  (setf (name-for-dtd sink) name))

(defmethod sax:notation-declaration ((sink sink) name public-id system-id)
  (when (and (canonical sink) (>= (canonical sink) 2))
    (let ((prev (previous-notation sink)))
      (cond
        (prev
          (unless (rod< prev name)
            (error "misordered notations; cannot unparse canonically")))
        (t
          ;; need a doctype declaration
          (write-rod #"<!DOCTYPE " sink)
          (write-rod (name-for-dtd sink) sink)
          (write-rod #" [" sink)
          (write-rune #/U+000A sink)))
      (setf (previous-notation sink) name)) 
    (write-rod #"<!NOTATION " sink)
    (write-rod name sink)
    (cond
      ((zerop (length public-id))
        (write-rod #" SYSTEM '" sink)
        (write-rod system-id sink)
        (write-rune #/' sink))
      ((zerop (length system-id))
        (write-rod #" PUBLIC '" sink)
        (write-rod public-id sink)
        (write-rune #/' sink))
      (t 
        (write-rod #" PUBLIC '" sink)
        (write-rod public-id sink)
        (write-rod #"' '" sink)
        (write-rod system-id sink)
        (write-rune #/' sink)))
    (write-rune #/> sink)
    (write-rune #/U+000A sink)))

(defmethod sax:end-dtd ((sink sink))
  (when (previous-notation sink)
    (write-rod #"]>" sink)
    (write-rune #/U+000A sink)))


;;;; elements

(defun sink-fresh-line (sink)
  (unless (zerop (column sink))
    (write-rune-0 10 sink)
    (indent sink)))

(defmethod sax:start-element
    ((sink sink) namespace-uri local-name qname attributes)
  (declare (ignore namespace-uri local-name))
  (when (stack sink)
    (incf (cdr (first (stack sink)))))
  (push (cons qname 0) (stack sink))
  (when (indentation sink)
    (sink-fresh-line sink)
    (start-indentation-block sink))
  (write-rune #/< sink)
  (write-rod qname sink)
  (let ((atts (sort (copy-list attributes) #'rod< :key #'sax:attribute-qname)))
    (dolist (a atts)
      (write-rune #/space sink)
      (write-rod (sax:attribute-qname a) sink)
      (write-rune #/= sink)
      (write-rune #/\" sink)
      (map nil (lambda (c) (unparse-datachar c sink)) (sax:attribute-value a))
      (write-rune #/\" sink)))
  (write-rod '#.(string-rod ">") sink))

(defmethod sax:end-element
    ((sink sink) namespace-uri local-name qname)
  (declare (ignore namespace-uri local-name))
  (let ((cons (pop (stack sink))))
    (unless (consp cons)
      (error "output does not nest: not in an element"))
    (unless (rod= (car cons) qname)
      (error "output does not nest: expected ~A but got ~A"
             (rod qname) (rod (car cons))))
    (when (indentation sink)
      (end-indentation-block sink)
      (unless (zerop (cdr cons))
        (sink-fresh-line sink))))
  (write-rod '#.(string-rod "</") sink)
  (write-rod qname sink)
  (write-rod '#.(string-rod ">") sink))

(defmethod sax:processing-instruction ((sink sink) target data)
  (unless (rod-equal target '#.(string-rod "xml"))
    (write-rod '#.(string-rod "<?") sink)
    (write-rod target sink)
    (write-rune #/space sink)
    (write-rod data sink)
    (write-rod '#.(string-rod "?>") sink)))

(defmethod sax:start-cdata ((sink sink))
  (push :cdata (stack sink)))

(defmethod sax:characters ((sink sink) data)
  (cond
    ((and (eq (car (stack sink)) :cdata)
          (not (canonical sink))
          (not (search #"]]" data)))
      (when (indentation sink)
        (sink-fresh-line sink))
      (write-rod #"<![CDATA[" sink)
      ;; XXX signal error if body is unprintable?
      (map nil (lambda (c) (write-rune c sink)) data)
      (write-rod #"]]>" sink))
    (t
      (if (indentation sink)
          (unparse-indented-text data sink)
          (map nil (if (canonical sink)
                       (lambda (c) (unparse-datachar c sink))
                       (lambda (c) (unparse-datachar-readable c sink)))
               data)))))

(defmethod sax:end-cdata ((sink sink))
  (unless (eq (pop (stack sink)) :cdata)
    (error "output does not nest: not in a cdata section")))

(defun indent (sink)
  (dotimes (x (current-indentation sink))
    (write-rune-0 32 sink)))

(defun start-indentation-block (sink)
  (incf (current-indentation sink) (indentation sink)))

(defun end-indentation-block (sink)
  (decf (current-indentation sink) (indentation sink)))

(defun unparse-indented-text (data sink)
  (flet ((whitespacep (x)
           (or (rune= x #/U+000A) (rune= x #/U+0020))))
    (let* ((n (length data))
           (pos (position-if-not #'whitespacep data))
           (need-whitespace-p nil))
      (cond
        ((zerop n))
        (pos
          (sink-fresh-line sink)
          (while (< pos n)
            (let* ((w (or (position-if #'whitespacep data :start (1+ pos)) n))
                   (next (or (position-if-not #'whitespacep data :start w) n)))
              (when need-whitespace-p
                (if (< (+ (column sink) w (- pos)) (width sink))
                    (write-rune-0 32 sink)
                    (sink-fresh-line sink)))
              (loop
                  for i from pos below w do
                    (unparse-datachar-readable (elt data i) sink))
              (setf need-whitespace-p (< w n))
              (setf pos next))))
        (t
          (write-rune-0 32 sink))))))

(defun unparse-datachar (c sink)
  (cond ((rune= c #/&) (write-rod '#.(string-rod "&amp;") sink))
        ((rune= c #/<) (write-rod '#.(string-rod "&lt;") sink))
        ((rune= c #/>) (write-rod '#.(string-rod "&gt;") sink))
        ((rune= c #/\") (write-rod '#.(string-rod "&quot;") sink))
        ((rune= c #/U+0009) (write-rod '#.(string-rod "&#9;") sink))
        ((rune= c #/U+000A) (write-rod '#.(string-rod "&#10;") sink))
        ((rune= c #/U+000D) (write-rod '#.(string-rod "&#13;") sink))
        (t
         (write-rune c sink))))

(defun unparse-datachar-readable (c sink)
  (cond ((rune= c #/&) (write-rod '#.(string-rod "&amp;") sink))
        ((rune= c #/<) (write-rod '#.(string-rod "&lt;") sink))
        ((rune= c #/>) (write-rod '#.(string-rod "&gt;") sink))
        ((rune= c #/\") (write-rod '#.(string-rod "&quot;") sink))
        (t
          (write-rune c sink))))


;;;; UTF-8 output for SINKs

(defun write-rod (rod sink)
  (map nil (lambda (c) (write-rune c sink)) rod))

(defun write-rune (rune sink)
  (let ((code (rune-code rune)))
    (with-slots (high-surrogate) sink
      (cond
        ((<= #xD800 code #xDBFF)
          (setf high-surrogate code))
        ((<= #xDC00 code #xDFFF)
          (let ((q (logior (ash (- high-surrogate #xD7C0) 10)
                           (- code #xDC00))))
            (write-rune-0 q sink))
          (setf high-surrogate nil))
        (t
          (write-rune-0 code sink))))))

(defun write-rune-0 (code sink)
  (labels ((wr (x)
             (write-octet x sink)))
    (cond ((<= #x00000000 code #x0000007F) 
           (wr code))
          ((<= #x00000080 code #x000007FF)
           (wr (logior #b11000000 (ldb (byte 5 6) code)))
           (wr (logior #b10000000 (ldb (byte 6 0) code))))
          ((<= #x00000800 code #x0000FFFF)
           (wr (logior #b11100000 (ldb (byte 4 12) code)))
           (wr (logior #b10000000 (ldb (byte 6 6) code)))
           (wr (logior #b10000000 (ldb (byte 6 0) code))))
          ((<= #x00010000 code #x001FFFFF)
           (wr (logior #b11110000 (ldb (byte 3 18) code)))
           (wr (logior #b10000000 (ldb (byte 6 12) code)))
           (wr (logior #b10000000 (ldb (byte 6 6) code)))
           (wr (logior #b10000000 (ldb (byte 6 0) code))))
          ((<= #x00200000 code #x03FFFFFF)
           (wr (logior #b11111000 (ldb (byte 2 24) code)))
           (wr (logior #b10000000 (ldb (byte 6 18) code)))
           (wr (logior #b10000000 (ldb (byte 6 12) code)))
           (wr (logior #b10000000 (ldb (byte 6 6) code)))
           (wr (logior #b10000000 (ldb (byte 6 0) code))))
          ((<= #x04000000 code #x7FFFFFFF)
           (wr (logior #b11111100 (ldb (byte 1 30) code)))
           (wr (logior #b10000000 (ldb (byte 6 24) code)))
           (wr (logior #b10000000 (ldb (byte 6 18) code)))
           (wr (logior #b10000000 (ldb (byte 6 12) code)))
           (wr (logior #b10000000 (ldb (byte 6 6) code)))
           (wr (logior #b10000000 (ldb (byte 6 0) code)))))))
