;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: CXML; readtable: runes; Encoding: utf-8; -*-
;;; ---------------------------------------------------------------------------
;;;     Title: A prototype XML parser
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

;;; Streams

;;; xstreams

;; For reading runes, I defined my own streams, called xstreams,
;; because we want to be fast. A function call or even a method call
;; per character is not acceptable, instead of that we define a
;; buffered stream with and advertised buffer layout, so that we
;; could use the trick stdio uses: READ-RUNE and PEEK-RUNE are macros,
;; directly accessing the buffer and only calling some underflow
;; handler in case of stream underflows. This will yield to quite a
;; performance boost vs calling READ-BYTE per character.

;; Also we need to do encoding and character set conversion on input,
;; this better done at large chunks of data rather than on a character
;; by character basis. This way we need a dispatch on the active
;; encoding only once in a while, instead of for each character. This
;; allows us to use a CLOS interface to do the underflow handling.

;;; zstreams

;; Now, for reading tokens, we define another kind of streams, called
;; zstreams. These zstreams also maintain an input stack to implement
;; inclusion of external entities. This input stack contains xstreams
;; or the special marker :STOP. Such a :STOP marker indicates, that
;; input should not continue there, but well stop; that is simulate an
;; EOF. The user is then responsible to pop this marker off the input
;; stack.
;;
;; This input stack is also used to detect circular entity inclusion.

;; The zstream tokenizer recognizes the following types of tokens and
;; is controlled by the *DATA-BEHAVIOUR* flag. (Which should become a
;; slot of zstreams instead).

;; Common
;;    :xml-pi (<target> . <content>)    ;processing-instruction starting with "<?xml"
;;    :pi (<target> . <content>)        ;processing-instruction
;;    :stag (<name> . <atts>)           ;start tag
;;    :etag (<name> . <atts>)           ;end tag
;;    :ztag (<name> . <atts>)           ;empty tag
;;    :<!element
;;    :<!entity
;;    :<!attlist
;;    :<!notation
;;    :<!doctype
;;    :<![
;;    :comment <content>

;; *data-behaviour* = :DTD
;;
;;    :name <interned-rod>
;;    :#required
;;    :#implied
;;    :#fixed
;;    :#pcdata
;;    :s
;;    :\[ :\] :\( :\) :|\ :\> :\" :\' :\, :\? :\* :\+ 

;; *data-behaviour* = :DOC
;;
;;    :entity-ref <interned-rod>
;;    :cdata <rod>




;;; NOTES
;;
;; Stream buffers as well as RODs are supposed to be encoded in
;; UTF-16. 

;; where does the time go?
;; DATA-RUNE-P
;; CANON-NOT-CDATA-ATTVAL
;; READ-ATTVAL (MUFFLE)
;; CLOSy DOM
;; UTF-8 decoder (13%)
;; READ-ATTVAL (10%)
;; 

;;; TODO
;;
;; o Improve error messages:
;;    - line and column number counters
;;    - better texts
;;    - better handling of errors (no crash'n burn behaviour)
;;
;; o provide for a faster DOM
;;
;; o morph zstream into a context object and thus also get rid of
;;   special variables. Put the current DTD there too.
;;   [mostly done]

;; o the *scratch-pad* hack should become something much more
;;   reentrant, we could either define a system-wide resource
;;   or allocate some scratch-pads per context.

;; o only parse the DTD on an option

;; o CR handling in utf-16 deocders
;;
;; o UCS-4 reader
;;
;; o max depth together with circle detection
;;   (or proof, that our circle detection is enough).
;;
;; o element definitions (with att definitions in the elements)
;;   [das haben wir doch, oder?]
;;
;; o store entities in the DTD
;;
;; o better extensibility wrt character representation, one may want to
;;   have
;;    - UTF-8  in standard CL strings
;;    - UCS-2  in RODs
;;    - UTF-16 in RODs
;;    - UCS-4  in vectoren
;;   [habe ich eigentlich nicht vor--david]
;;
;; o xstreams auslagern, documententieren und dann auch in SGML und
;;   CSS parser verwenden. (halt alles was zeichen liest).
::   [ausgelagert sind sie; dokumentiert "so la la"; die Reintegration
;;   in Closure ist ein ganz anderes Thema]
;;
;; o merge node representation with SGML module
;;   [???]
;; 
;; o line/column number recording
;;
;; o better error messages
;;
;; o recording of source locations for nodes.
;;
;; o make the *scratch-pad* hack safe
;;
;; o based on the DTD and xml:space attribute implement HTML white
;;   space rules.
;;
;; o on a parser option, do not expand external entities.
;;
;; o on a parser option, do not parse the DTD.
;;
;; o caching DTDs?
;;
;;   That is, if we parse a lot of documents all having the same DTD,
;;   we do not need to re-read it every time.
;;   But watch the file write date, since not doing so would be
;;   a good way to confuse a hell lot of users.
;;   But: What to do with declarations in the <!DOCTYPE header?
;;
;;
;; o does the user need the distinction between "&#20;" and " " ?
;;   That is literal and 'quoted' white space.
;;   [verstehe ich nicht --david]
;;
;; o on an option merge CDATA section;
;;
;; o data in parse tree? extra nodes like in SGML?!
;;
;; o what to store in the node-gi field? Some name object or the
;;   string used?
;;

;; Test that fail:
;;
;; not-wf/sa/128        is false a alarm
;;

;;;; Validity constraints:
;;;; (00) Root Element Type                     like (03), c.f. MAKE-ROOT-MODEL
;;;; (01) Proper Declaration/PE Nesting         P/MARKUP-DECL
;;;; (02) Standalone Document Declaration       all over the place [*]
;;;; (03) Element Valid                         VALIDATE-*-ELEMENT, -CHARACTERS
;;;; (04) Attribute Value Type                  VALIDATE-ATTRIBUTE
;;;; (05) Unique Element Type Declaration       DEFINE-ELEMENT
;;;; (06) Proper Group/PE Nesting               P/CSPEC
;;;; (07) No Duplicate Types                    LEGAL-CONTENT-MODEL-P
;;;; (08) ID                                    VALIDATE-ATTRIBUTE
;;;; (09) One ID per Element Type               DEFINE-ATTRIBUTE
;;;; (10) ID Attribute Default                  DEFINE-ATTRIBUTE
;;;; (11) IDREF                                 VALIDATE-ATTRIBUTE, P/DOCUMENT
;;;; (12) Entity Name                           VALIDATE-ATTRIBUTE
;;;; (13) Name Token                            VALIDATE-ATTRIBUTE
;;;; (14) Notation Attributes                   VALIDATE-ATTRIBUTE, P/ATT-TYPE
;;;; (15) One Notation Per Element Type         DEFINE-ATTRIBUTE
;;;; (16) No Notation on Empty Element          DEFINE-ELEMENT, -ATTRIBUTE
;;;; (17) Enumeration                           VALIDATE-ATTRIBUTE
;;;; (18) Required Attribute                    PROCESS-ATTRIBUTES
;;;; (19) Attribute Default Legal               DEFINE-ATTRIBUTE
;;;; (20) Fixed Attribute Default               VALIDATE-ATTRIBUTE
;;;; (21) Proper Conditional Section/PE Nesting P/CONDITIONAL-SECT, ...
;;;; (22) Entity Declared                       [**]
;;;; (23) Notation Declared                     P/ENTITY-DEF, P/DOCUMENT
;;;; (24) Unique Notation Name                  DEFINE-NOTATION
;;;;
;;;; [*] Perhaps we could revert the explicit checks of (02), if we did
;;;; _not_ read external subsets of standalone documents when parsing in
;;;; validating mode.  Violations of VC (02) constraints would then appear as
;;;; wellformedness violations, right?
;;;;
;;;; [**] Although I haven't investigated this properly yet, I believe that
;;;; we check this VC together with the WFC even in non-validating mode.

(in-package :cxml)

#+allegro
(setf (excl:named-readtable :runes) *readtable*)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *fast* '(optimize (speed 3) (safety 0)))
  ;;(defparameter *fast* '(optimize (speed 2) (safety 3)))
  )

;;; parser context

(defvar *ctx*)

(defstruct (context (:conc-name nil))
  handler
  (namespace-bindings *default-namespace-bindings*)
  (dtd (make-dtd))
  model-stack
  (referenced-notations '())
  (id-table (%make-rod-hash-table))
  (standalone-p nil))

(defvar *expand-pe-p*)

;;;; ---------------------------------------------------------------------------
;;;; xstreams
;;;;


(defstruct (stream-name (:type list))
  entity-name
  entity-kind
  file-name)

(defun print-xstream (self sink depth)
  (declare (ignore depth))
  (format sink "#<~S ~S>" (type-of self) (mu (xstream-name self))))

(deftype read-element () 'rune)

(defun call-with-open-xstream (continuation &rest open-args)
  (let ((input (apply #'open (car open-args) :element-type '(unsigned-byte 8) (cdr open-args))))
    (unwind-protect
        (progn
          (funcall continuation (make-xstream input)))
      (close input))))

(defmacro with-open-xstream ((stream &rest open-args) &body body)
  `(call-with-open-xstream (lambda (,stream) .,body) .,open-args))

;;; Decoders

;; The decoders share a common signature:
;;
;; DECODE input input-start input-end
;;        output output-start output-end
;;        eof-p
;; -> first-not-written ; first-not-read
;;
;; These decode functions should decode as much characters off `input'
;; into the `output' as possible and return the indexes to the first
;; not read and first not written element of `input' and `output'
;; respectively.  If there are not enough bytes in `input' to decode a
;; full character, decoding shold be abandomed; the caller has to
;; ensure that the remaining bytes of `input' are passed to the
;; decoder again with more bytes appended. 
;;
;; `eof-p' now in turn indicates, if the given input sequence, is all
;; the producer does have and might be used to produce error messages
;; in case of incomplete codes or decided what to do.
;;
;; Decoders are expected to handle the various CR/NL conventions and
;; canonicalize each end of line into a single NL rune (#xA) in good
;; old Lisp tradition.
;;

;; TODO: change this to an encoding class, which then might carry
;; additional state. Stateless encodings could been represented by
;; keywords. e.g.
;;
;;  defmethod DECODE-SEQUENCE ((encoding (eql :utf-8)) ...)
;;

;;;; -------------------------------------------------------------------
;;;; Rechnen mit Runen
;;;;

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

;;; XXX Geschwindigkeit dieser Definitionen untersuchen!

(defmacro rune-op (op &rest xs) 
  `(code-rune (,op ,@(mapcar (lambda (x) `(rune-code ,x)) xs))))
(defmacro rune-pred (op &rest xs) 
  `(,op ,@(mapcar (lambda (x) `(rune-code ,x)) xs)))

(defmacro %rune+   (&rest xs) `(rune-op + ,@xs))
(defmacro %rune-   (&rest xs) `(rune-op - ,@xs))
(defmacro %rune*   (&rest xs) `(rune-op * ,@xs))
(defmacro %rune/   (&rest xs) `(rune-op floor ,@xs))
(defmacro %rune-and (&rest xs) `(rune-op logand ,@xs))
(defmacro %rune-ior (&rest xs) `(rune-op logior ,@xs))
(defmacro %rune-xor (&rest xs) `(rune-op logxor ,@xs))
(defmacro %rune-ash (a b) `(code-rune (ash (rune-code ,a) ,b)))
(defmacro %rune-mod (&rest xs) `(rune-op mod ,@xs))

(defmacro %rune=  (&rest xs)  `(rune-pred = ,@xs))
(defmacro %rune<= (&rest xs)  `(rune-pred <= ,@xs))
(defmacro %rune>= (&rest xs)  `(rune-pred >= ,@xs))
(defmacro %rune<  (&rest xs)  `(rune-pred < ,@xs))
(defmacro %rune>  (&rest xs)  `(rune-pred > ,@xs))

;;;; ---------------------------------------------------------------------------
;;;; rod hashtable
;;;;

;;; make-rod-hashtable
;;; rod-hash-get hashtable rod &optional start end -> value ; successp
;;; (setf (rod-hash-get hashtable rod &optional start end) new-value
;;; 

(defstruct (rod-hashtable (:constructor make-rod-hashtable/low))
  size          ;size of table
  table         ;
  )

(defun make-rod-hashtable (&key (size 200))
  (setf size (runes::nearest-greater-prime size))
  (make-rod-hashtable/low
   :size size
   :table (make-array size :initial-element nil)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +fixnum-bits+
      (1- (integer-length most-positive-fixnum))
    "Pessimistic approximation of the number of bits of fixnums.")

  (defconstant +fixnum-mask+
      (1- (expt 2 +fixnum-bits+))
    "Pessimistic approximation of the largest bit-mask, still being a fixnum."))

(defsubst stir (a b)
  (%and +fixnum-mask+
        (%xor (%ior (%ash (%and a #.(ash +fixnum-mask+ -5)) 5)
                    (%ash a #.(- 5 +fixnum-bits+)))
              b)))

(defsubst rod-hash (rod start end)
  "Compute a hash code out of a rod."
  (let ((res (%- end start)))
    (do ((i start (%+ i 1)))
        ((%= i end))
      (declare (type fixnum i))
      (setf res (stir res (rune-code (%rune rod i)))))
    res))

(defsubst rod=* (x y &key (start1 0) (end1 (length x))
                          (start2 0) (end2 (length y)))
  (and (%= (%- end1 start1) (%- end2 start2))
       (do ((i start1 (%+ i 1))
            (j start2 (%+ j 1)))
           ((%= i end1)
            t)
         (unless (rune= (%rune x i) (%rune y j))
           (return nil)))))

(defsubst rod=** (x y start1 end1 start2 end2)
  (and (%= (%- end1 start1) (%- end2 start2))
       (do ((i start1 (%+ i 1))
            (j start2 (%+ j 1)))
           ((%= i end1)
            t)
         (unless (rune= (%rune x i) (%rune y j))
           (return nil)))))

(defun rod-hash-get (hashtable rod &optional (start 0) (end (length rod)))
  (declare (type (simple-array rune (*)) rod))
  (let ((j (%mod (rod-hash rod start end)
                 (rod-hashtable-size hashtable))))
    (dolist (q (svref (rod-hashtable-table hashtable) j)
               (values nil nil nil))
      (declare (type cons q))
      (when (rod=** (car q) rod 0 (length (the (simple-array rune (*)) (car q))) start end)
        (return (values (cdr q) t (car q)))))))

(defun rod-hash-set (new-value hashtable rod &optional (start 0) (end (length rod)))
  (let ((j (%mod (rod-hash rod start end)
                 (rod-hashtable-size hashtable)))
        (key nil))
    (dolist (q (svref (rod-hashtable-table hashtable) j)
              (progn
                (setf key (rod-subseq* rod start end))
                (push (cons key new-value)
                      (aref (rod-hashtable-table hashtable) j))))
      (when (rod=* (car q) rod :start2 start :end2 end)
        (setf key (car q))
        (setf (cdr q) new-value)
        (return)))
    (values new-value key)))

#-rune-is-character
(defun rod-subseq* (source start &optional (end (length source)))
  (unless (and (typep start 'fixnum) (>= start 0))
    (error "~S is not a non-negative fixnum." start))
  (unless (and (typep end 'fixnum) (>= end start))
    (error "END argument, ~S, is not a fixnum no less than START, ~S." end start))
  (when (> start (length source))
    (error "START argument, ~S, should be no greater than length of rod." start))
  (when (> end (length source))
    (error "END argument, ~S, should be no greater than length of rod." end))
  (locally
      (declare (type fixnum start end))
    (let ((res (make-rod (- end start))))
      (declare (type rod res))
      (do ((i (- (- end start) 1) (the fixnum (- i 1))))
          ((< i 0) res)
        (declare (type fixnum i))
        (setf (%rune res i) (aref source (the fixnum (+ i start))))))))

#+rune-is-character
(defun rod-subseq* (source start &optional (end (length source)))
  (subseq source start end))

(deftype ufixnum () `(unsigned-byte ,(integer-length most-positive-fixnum)))

#-rune-is-character
(defun rod-subseq** (source start &optional (end (length source)))
  (declare (type (simple-array rune (*)) source)
           (type ufixnum start)
           (type ufixnum end)
           (optimize (speed 3) (safety 0)))
  (let ((res (make-array (%- end start) :element-type 'rune)))
    (declare (type (simple-array rune (*)) res))
    (let ((i (%- end start)))
      (declare (type ufixnum i))
      (loop
        (setf i (- i 1))
        (when (= i 0)
          (return))
        (setf (%rune res i) (%rune source (the ufixnum (+ i start))))))
    res))

#+rune-is-character
(defun rod-subseq** (source start &optional (end (length source)))
  (subseq source start end))

(defun (setf rod-hash-get) (new-value hashtable rod &optional (start 0) (end (length rod)))
  (rod-hash-set new-value hashtable rod start end))

(defparameter *name-hashtable* (make-rod-hashtable :size 2000))

(defun intern-name (rod &optional (start 0) (end (length rod)))
  (multiple-value-bind (value successp key) (rod-hash-get *name-hashtable* rod start end)
    (declare (ignore value))
    (if successp
        key
      (nth-value 1 (rod-hash-set t *name-hashtable* rod start end)))))

;;;; ---------------------------------------------------------------------------
;;;;
;;;;  rod collector
;;;;

(defparameter *scratch-pad*
    (make-array 1024 :element-type 'rune))

(defparameter *scratch-pad-2*
    (make-array 1024 :element-type 'rune))

(defparameter *scratch-pad-3*
    (make-array 1024 :element-type 'rune))

(defparameter *scratch-pad-4*
    (make-array 1024 :element-type 'rune))

(declaim (type (simple-array rune (*))
               *scratch-pad* *scratch-pad-2* *scratch-pad-3* *scratch-pad-4*))

(defmacro %put-unicode-char (code-var put)
  `(progn
     (cond ((%> ,code-var #xFFFF)
          (,put (the rune (code-rune (%+ #xD7C0 (%ash ,code-var -10)))))
          (,put (the rune (code-rune (%ior #xDC00 (%and ,code-var #x03FF))))))
         (t
          (,put (code-rune ,code-var))))))

(defun adjust-array-by-copying (old-array new-size)
  "Adjust an array by copying and thus ensures, that result is a SIMPLE-ARRAY."
  (let ((res (make-array new-size :element-type (array-element-type old-array))))
    (replace res old-array
             :start1 0 :end1 (length old-array)
             :start2 0 :end2 (length old-array))
    res))

(defmacro with-rune-collector-aux (scratch collect body mode)
  (let ((rod (gensym))
        (n (gensym))
        (i (gensym))
        (b (gensym)))
    `(let ((,n (length ,scratch))
           (,i 0)
           (,b ,scratch))
       (declare (type fixnum ,n ,i))
       (macrolet 
           ((,collect (x) 
              `((lambda (x)
                  (locally
                      (declare #.*fast*)
                    (when (%>= ,',i ,',n)
                      (setf ,',n (* 2 ,',n))
                      (setf ,',b
                            (setf ,',scratch
                                  (adjust-array-by-copying ,',scratch ,',n))))
                    (setf (aref (the (simple-array rune (*)) ,',b) ,',i) x)
                    (incf ,',i)))
                ,x)))
         ,@body
         ,(ecase mode
            (:intern
             `(intern-name ,b 0 ,i))
            (:copy
             `(let ((,rod (make-rod ,i)))
                (while (not (%= ,i 0))
                       (setf ,i (%- ,i 1))
                       (setf (%rune ,rod ,i) 
                         (aref (the (simple-array rune (*)) ,b) ,i)))
                ,rod))
            (:raw
             `(values ,b 0 ,i))
            )))))

'(defmacro with-rune-collector-aux (scratch collect body mode)
  (let ((rod (gensym))
        (n (gensym))
        (i (gensym))
        (b (gensym)))
    `(let ((,n (length ,scratch))
           (,i 0))
       (declare (type fixnum ,n ,i))
       (macrolet 
           ((,collect (x) 
              `((lambda (x)
                  (locally
                      (declare #.*fast*)
                    (when (%>= ,',i ,',n)
                      (setf ,',n (* 2 ,',n))
                      (setf ,',scratch
                            (setf ,',scratch
                                  (adjust-array-by-copying ,',scratch ,',n))))
                    (setf (aref (the (simple-array rune (*)) ,',scratch) ,',i) x)
                    (incf ,',i)))
                ,x)))
         ,@body
         ,(ecase mode
            (:intern
             `(intern-name ,scratch 0 ,i))
            (:copy
             `(let ((,rod (make-rod ,i)))
                (while (%> ,i 0)
                       (setf ,i (%- ,i 1))
                       (setf (%rune ,rod ,i) 
                         (aref (the (simple-array rune (*)) ,scratch) ,i)))
                ,rod))
            (:raw
             `(values ,scratch 0 ,i))
            )))))

(defmacro with-rune-collector ((collect) &body body)
  `(with-rune-collector-aux *scratch-pad* ,collect ,body :copy))

(defmacro with-rune-collector-2 ((collect) &body body)
  `(with-rune-collector-aux *scratch-pad-2* ,collect ,body :copy))

(defmacro with-rune-collector-3 ((collect) &body body)
  `(with-rune-collector-aux *scratch-pad-3* ,collect ,body :copy))

(defmacro with-rune-collector-4 ((collect) &body body)
  `(with-rune-collector-aux *scratch-pad-4* ,collect ,body :copy))

(defmacro with-rune-collector/intern ((collect) &body body)
  `(with-rune-collector-aux *scratch-pad* ,collect ,body :intern))

(defmacro with-rune-collector/raw ((collect) &body body)
  `(with-rune-collector-aux *scratch-pad* ,collect ,body :raw))

#|
(defmacro while-reading-runes ((reader stream-in) &rest body)
  ;; Thou shalt not leave body via a non local exit
  (let ((stream (make-symbol "STREAM"))
        (rptr (make-symbol "RPTR"))
        (fptr (make-symbol "FPTR"))
        (buf  (make-symbol "BUF")) )
    `(let* ((,stream ,stream-in)
            (,rptr (xstream-read-ptr ,stream))
            (,fptr (xstream-fill-ptr ,stream))
            (,buf  (xstream-buffer ,stream)))
       (declare (type fixnum ,rptr ,fptr)
                (type xstream ,stream))
       (macrolet ((,reader (res-var)
                    `(cond ((%= ,',rptr ,',fptr)
                            (setf (xstream-read-ptr ,',stream) ,',rptr)
                            (setf ,res-var (xstream-underflow ,',stream))
                            (setf ,',rptr (xstream-read-ptr ,',stream))
                            (setf ,',fptr (xstream-fill-ptr ,',stream))
                            (setf ,',buf  (xstream-buffer ,',stream)))
                           (t
                            (setf ,res-var
                              (aref (the (simple-array read-element (*)) ,',buf)
                                    (the fixnum ,',rptr)))
                            (setf ,',rptr (%+ ,',rptr 1))))))
         (prog1
             (let () .,body)
           (setf (xstream-read-ptr ,stream) ,rptr) )))))
|#

;;;;  ---------------------------------------------------------------------------
;;;;  DTD
;;;;

(define-condition validity-error (simple-error) ())

(defun validity-error (x &rest args)
  (error 'validity-error
         :format-control "Validity constraint violated: ~@?"
         :format-arguments (list x args)))

(defvar *validate* t)
(defvar *markup-declaration-external-p* nil)

(defun validate-start-element (ctx name)
  (when *validate*
    (let* ((pair (car (model-stack ctx)))
           (newval (funcall (car pair) name)))
      (unless newval
        (validity-error "(03) Element Valid: ~A" (rod-string name)))
      (setf (car pair) newval)
      (let ((e (find-element name (dtd ctx))))
        (unless e
          (validity-error "(03) Element Valid: no definition for ~A"
                          (rod-string name)))
        (maybe-compile-cspec e)
        (push (copy-cons (elmdef-compiled-cspec e)) (model-stack ctx))))))

(defun copy-cons (x)
  (cons (car x) (cdr x)))

(defun validate-end-element (ctx name)
  (when *validate*
    (let ((pair (car (model-stack ctx))))
      (unless (eq (funcall (car pair) nil) t)
        (validity-error "(03) Element Valid: ~A" (rod-string name)))
      (pop (model-stack ctx)))))

(defun validate-characters (ctx rod)
  (when *validate*
    (let ((pair (car (model-stack ctx))))
      (unless (funcall (cdr pair) rod)
        (validity-error "(03) Element Valid: unexpected PCDATA")))))

(defun standalone-check-necessary-p (def)
  (and *validate*
       (standalone-p *ctx*)
       (etypecase def
         (elmdef (elmdef-external-p def))
         (attdef (attdef-external-p def)))))

(defstruct attribute
  namespace-uri
  local-name
  qname
  value
  specified-p)

(defun process-attributes (ctx name attlist)
  (let ((e (find-element name (dtd ctx))))
    (cond
      (e
        (dolist (ad (elmdef-attributes e)) ;handle default values
          (unless (get-attribute (attdef-name ad) attlist)
            (case (attdef-default ad)
              (:IMPLIED)
              (:REQUIRED
                (when *validate*
                  (validity-error "(18) Required Attribute: ~S not specified"
                                  (rod-string (attdef-name ad)))))
              (t
                (when (standalone-check-necessary-p ad)
                  (validity-error "(02) Standalone Document Declaration: missing attribute value"))
                (push (build-attribute (attdef-name ad)
                                       (cadr (attdef-default ad))
                                       nil)
                      attlist)))))
        (dolist (a attlist)             ;normalize non-CDATA values
          (let* ((qname (attribute-qname a))
                 (adef (find-attribute e qname)))
            (when (and adef (not (eq (attdef-type adef) :CDATA)))
              (let ((canon (canon-not-cdata-attval (attribute-value a))))
                (when (and (standalone-check-necessary-p adef)
                           (not (rod= (attribute-value a) canon)))
                  (validity-error "(02) Standalone Document Declaration: attribute value not normalized"))
                (setf (attribute-value a) canon)))))
        (when *validate*                ;maybe validate attribute values
          (dolist (a attlist)
            (validate-attribute ctx e a))))
      ((and *validate* attlist)
        (validity-error "(04) Attribute Value Type: no definition for element ~A"
                        (rod-string name)))))
  attlist)

(defun get-attribute (name attributes)
  (member name attributes :key #'attribute-qname :test #'rod=))

(defun validate-attribute (ctx e a)
  (when (attribute-specified-p a)       ;defaults checked by DEFINE-ATTRIBUTE
    (let* ((qname (attribute-qname a))
           (adef
            (or (find-attribute e qname)
                (validity-error "(04) Attribute Value Type: not declared: ~A"
                                (rod-string qname)))))
      (validate-attribute* ctx adef (attribute-value a)))))

(defun validate-attribute* (ctx adef value)
  (let ((type (attdef-type adef))
        (default (attdef-default adef))) 
    (when (and (listp default)
               (eq (car default) :FIXED)
               (not (rod= value (cadr default))))
      (validity-error "(20) Fixed Attribute Default: expected ~S but got ~S"
                      (rod-string (cadr default))
                      (rod-string value)))
    (ecase (if (listp type) (car type) type)
      (:ID
        (unless (valid-name-p value)
          (validity-error "(08) ID: not a name: ~S" (rod-string value)))
        (when (eq (gethash value (id-table ctx)) t)
          (validity-error "(08) ID: ~S not unique" (rod-string value)))
        (setf (gethash value (id-table ctx)) t))
      (:IDREF
        (validate-idref ctx value))
      (:IDREFS
        (let ((names (split-names value)))
          (unless names
            (validity-error "(11) IDREF: malformed names"))
          (mapc (curry #'validate-idref ctx) names)))
      (:NMTOKEN
        (validate-nmtoken value))
      (:NMTOKENS
        (let ((tokens (split-names value)))
          (unless tokens
            (validity-error "(13) Name Token: malformed NMTOKENS"))
          (mapc #'validate-nmtoken tokens)))
      (:ENUMERATION
        (unless (member value (cdr type) :test #'rod=)
          (validity-error "(17) Enumeration: value not declared: ~S"
                          (rod-string value))))
      (:NOTATION
        (unless (member value (cdr type) :test #'rod=)
          (validity-error "(14) Notation Attributes: ~S" (rod-string value))))
      (:ENTITY
        (validate-entity value))
      (:ENTITIES
        (let ((names (split-names value)))
          (unless names
            (validity-error "(13) Name Token: malformed NMTOKENS"))
          (mapc #'validate-entity names)))
      (:CDATA))))

(defun validate-idref (ctx value)
  (unless (valid-name-p value)
    (validity-error "(11) IDREF: not a name: ~S" (rod-string value)))
  (unless (gethash value (id-table ctx))
    (setf (gethash value (id-table ctx)) nil)))

(defun validate-nmtoken (value)
  (unless (valid-nmtoken-p value)
    (validity-error "(13) Name Token: not a NMTOKEN: ~S"
                    (rod-string value))))

(defun validate-entity (value)
  (unless (valid-name-p value)
    (validity-error "(12) Entity Name: not a name: ~S" (rod-string value)))
  (let ((def (let ((*validate*
                    ;; Similarly the entity refs are internal and
                    ;; don't need normalization ... the unparsed
                    ;; entities (and entities) aren't "references"
                    ;;   -- sun/valid/sa03.xml
                    nil))
               (get-entity-definition value :general (dtd *ctx*)))))
    (unless (and def (third def))       ;unparsed entity
      (validity-error "(12) Entity Name: ~S" (rod-string value)))))

(defun split-names (rod)
  (flet ((whitespacep (x)
           (or (rune= x #/U+0009)
               (rune= x #/U+000A)
               (rune= x #/U+000D)
               (rune= x #/U+0020))))
    (if (let ((n (length rod)))
          (and (not (zerop n))
               (or (whitespacep (rune rod 0))
                   (whitespacep (rune rod (1- n))))))
        nil
        (split-sequence-if #'whitespacep rod :remove-empty-subseqs t))))

(defun absolute-uri (sysid source-stream)
  (setq sysid (rod-string sysid))
  (let ((base-sysid
         (dolist (k (zstream-input-stack source-stream))
           (let ((base-sysid (stream-name-file-name (xstream-name k))))
             (when base-sysid (return base-sysid))))))
    (assert (not (null base-sysid)))
    (merge-sysid sysid base-sysid)))

(defun absolute-extid (source-stream extid)
  (case (car extid)
    (:SYSTEM
     (list (car extid)
           (absolute-uri (cadr extid) source-stream)))
    (:PUBLIC
     (list (car extid)
           (cadr extid)
           (absolute-uri (caddr extid) source-stream)))))

(defun define-entity (source-stream name kind def)
  (when (eq (car def) :EXTERNAL)
    (setf def
      (list (car def) (absolute-extid source-stream (cadr def)) (third def))))
  (setf name (intern-name name))
  (let ((table
         (ecase kind
           (:general (dtd-gentities (dtd *ctx*)))
           (:parameter (dtd-pentities (dtd *ctx*))))))
    (unless (gethash name table)
      (setf (gethash name table)
            (cons *markup-declaration-external-p* def)))))

(defun get-entity-definition (entity-name kind dtd)
  (destructuring-bind (extp &rest def)
      (gethash entity-name
               (ecase kind
                 (:general (dtd-gentities dtd))
                 (:parameter (dtd-pentities dtd)))
               '(nil))
    (when (and *validate* (standalone-p *ctx*) extp)
      (validity-error "(02) Standalone Document Declaration: entity reference: ~S"
                      (rod-string entity-name)))
    def))

(defun entity->xstream (entity-name kind &optional zstream)
  ;; `zstream' is for error messages
  (let ((def (get-entity-definition entity-name kind (dtd *ctx*))))
    (unless def
      (if zstream 
          (perror zstream "Entity '~A' is not defined." (rod-string entity-name))
        (error "Entity '~A' is not defined." (rod-string entity-name))))
    (let (r)
      (ecase (car def)
        (:INTERNAL 
         (setf r (make-rod-xstream (cadr def)))
         (setf (xstream-name r)
           (make-stream-name :entity-name entity-name
                             :entity-kind kind
                             :file-name nil)))
        (:EXTERNAL
         (setf r (open-extid (cadr def)))
         (setf (stream-name-entity-name (xstream-name r)) entity-name
               (stream-name-entity-kind (xstream-name r)) kind)))
      r)))

(defun entity-source-kind (name type)
  (let ((def (get-entity-definition name type (dtd *ctx*))))
    (unless def
      (error "Entity '~A' is not defined." (rod-string name)))
    (car def)))

(defun open-extid (extid)
  (let ((nam (ecase (car extid)
               (:SYSTEM (cadr extid))
               (:PUBLIC (caddr extid)))))
    (make-xstream (open-sysid nam)
                  :name (make-stream-name :file-name nam)
                  :initial-speed 1)))

(defun call-with-entity-expansion-as-stream (zstream cont name kind)
  ;; `zstream' is for error messages -- we need something better!
  (let ((in (entity->xstream name kind zstream)))
    (unwind-protect
        (funcall cont in)
      (close-xstream in))))

(defun define-default-entities ()
  (define-entity nil '#.(string-rod "lt")   :general `(:INTERNAL #.(string-rod "&#60;")))
  (define-entity nil '#.(string-rod "gt")   :general `(:INTERNAL #.(string-rod ">")))
  (define-entity nil '#.(string-rod "amp")  :general `(:INTERNAL #.(string-rod "&#38;")))
  (define-entity nil '#.(string-rod "apos") :general `(:INTERNAL #.(string-rod "'")))
  (define-entity nil '#.(string-rod "quot") :general `(:INTERNAL #.(string-rod "\"")))
  ;;
  #||
  (define-entity nil '#.(string-rod "ouml") :general `(:INTERNAL #.(string-rod "ö")))
  (define-entity nil '#.(string-rod "uuml") :general `(:INTERNAL #.(string-rod "ü")))
  (define-entity nil '#.(string-rod "auml") :general `(:INTERNAL #.(string-rod "ä")))
  (define-entity nil '#.(string-rod "Ouml") :general `(:INTERNAL #.(string-rod "Ö")))
  (define-entity nil '#.(string-rod "Auml") :general `(:INTERNAL #.(string-rod "Ä")))
  (define-entity nil '#.(string-rod "Uuml") :general `(:INTERNAL #.(string-rod "Ü")))
  (define-entity nil '#.(string-rod "szlig") :general `(:INTERNAL #.(string-rod "ß")))
  ||#
  ;;
  #||
  (define-entity nil '#.(string-rod "nbsp") 
    :general `(:INTERNAL ,(let ((r (make-rod 1)))
                            (setf (aref r 0) #o240)
                            r)))
  ||#
  )

(defstruct attdef
  ;; an attribute definition
  element       ;name of element this attribute belongs to
  name          ;name of attribute
  type          ;type of attribute; either one of :CDATA, :ID, :IDREF, :IDREFS,
                ; :ENTITY, :ENTITIES, :NMTOKEN, :NMTOKENS, or
                ; (:NOTATION <name>*)
                ; (:ENUMERATION <name>*)
  default       ;default value of attribute:
                ; :REQUIRED, :IMPLIED, (:FIXED content) or (:DEFAULT content)
  (external-p *markup-declaration-external-p*)
  )

(defstruct elmdef
  ;; an element definition
  name          ;name of the element
  content       ;content model            [*]
  attributes    ;list of defined attributes
  compiled-cspec ;cons of validation function for contentspec
  (external-p *markup-declaration-external-p*)
  )

;; [*] in XML it is possible to define attributes before the element
;; itself is defined and since we hang attribute definitions into the
;; relevant element definitions, the `content' slot indicates whether an
;; element was actually defined.  It is NIL until set to a content model
;; when the element type declaration is processed.

(defun %make-rod-hash-table ()
  ;; XXX with portable hash tables, this is the only way to case-sensitively
  ;; use rods.  However, EQUALP often has horrible performance!  Most Lisps
  ;; provide extensions for user-defined equality, we should use them!  There
  ;; is also a home-made hash table for rods defined below, written by
  ;; Gilbert (I think).  We could also use that one, but I would prefer the
  ;; first method, even if it's unportable.
  (make-hash-table :test
                   #+rune-is-character 'equal
                   #-rune-is-character 'equalp))

(defstruct dtd
  (elements (%make-rod-hash-table))     ;elmdefs
  (gentities (%make-rod-hash-table))    ;general entities
  (pentities (%make-rod-hash-table))    ;parameter entities
  (notations (%make-rod-hash-table))
  )

;;;;

(defun find-element (name dtd)
  (gethash name (dtd-elements dtd)))

(defun define-element (dtd element-name &optional content-model)
  (let ((e (find-element element-name dtd)))
    (cond
      ((null e)
        (setf (gethash element-name (dtd-elements dtd))
              (make-elmdef :name element-name :content content-model)))
      ((null content-model)
        e)
      (t
        (when *validate*
          (when (elmdef-content e)
            (validity-error "(05) Unique Element Type Declaration"))
          (when (eq content-model :EMPTY)
            (dolist (ad (elmdef-attributes e))
              (let ((type (attdef-type ad)))
                (when (and (listp type) (eq (car type) :NOTATION))
                  (validity-error "(16) No Notation on Empty Element: ~S"
                                  (rod-string element-name)))))))
        (setf (elmdef-content e) content-model)
        (setf (elmdef-external-p e) *markup-declaration-external-p*)
        e))))

(defvar *redefinition-warning* t)

(defun define-attribute (dtd element name type default)
  (let ((adef (make-attdef :element element
                           :name name
                           :type type
                           :default default))
        (e (or (find-element element dtd)
               (define-element dtd element))))
    (cond ((find-attribute e name)
           (when *redefinition-warning*
             (warn "Attribute \"~A\" of \"~A\" not redefined."
                   (rod-string name)
                   (rod-string element))))
          (t
           (when *validate*
             (when (eq type :ID)
               (when (find :ID (elmdef-attributes e) :key #'attdef-type)
                 (validity-error "(09) One ID per Element Type: element ~A"
                                 (rod-string element)))
               (unless (member default '(:REQUIRED :IMPLIED))
                 (validity-error "(10) ID Attribute Default: ~A"
                                 (rod-string element))))
             (flet ((notationp (type)
                      (and (listp type) (eq (car type) :NOTATION))))
               (when (notationp type)
                 (when (find-if #'notationp (elmdef-attributes e)
                                :key #'attdef-type)
                   (validity-error "(15) One Notation Per Element Type: ~S"
                                   (rod-string element)))
                 (when (eq (elmdef-content e) :EMPTY)
                   (validity-error "(16) No Notation on Empty Element: ~S"
                                   (rod-string element))))))
           (push adef (elmdef-attributes e))))
    (when (and *validate* (listp default))
      (unless (eq (attdef-type adef) :CDATA)
        (setf (second default) (canon-not-cdata-attval (second default))))
      (validate-attribute* *ctx* adef (second default)))))

(defun find-attribute (elmdef name)
  (find name (elmdef-attributes elmdef) :key #'attdef-name :test #'rod=))

(defun define-notation (dtd name id)
  (let ((ns (dtd-notations dtd)))
    (when (gethash name ns)
      (validity-error "(24) Unique Notation Name: ~S" (rod-string name)))
    (setf (gethash name ns) id)))

(defun find-notation (name dtd)
  (gethash name (dtd-notations dtd)))

;;;; ---------------------------------------------------------------------------
;;;;  z streams and lexer
;;;;

(defstruct zstream
  token-category
  token-semantic
  input-stack)

(defun read-token (input)
  (cond ((zstream-token-category input)
         (multiple-value-prog1
             (values (zstream-token-category input)
                     (zstream-token-semantic input))
           (setf (zstream-token-category input) nil
                 (zstream-token-semantic input) nil)))
        (t
         (read-token-2 input))))

(defun peek-token (input)
  (cond ((zstream-token-category input)
         (values 
          (zstream-token-category input)
          (zstream-token-semantic input)))
        (t
         (multiple-value-bind (c s) (read-token input)
           (setf (zstream-token-category input) c
                 (zstream-token-semantic input) s))
         (values (zstream-token-category input)
                 (zstream-token-semantic input)))))

(defun read-token-2 (input)
  (cond ((null (zstream-input-stack input))
         (values :eof nil))
        (t
         (let ((c (peek-rune (car (zstream-input-stack input)))))
           (cond ((eq c :eof)
                  (cond ((eq (cadr (zstream-input-stack input)) :stop)
                         (values :eof nil))
                        (t
                         (close-xstream (pop (zstream-input-stack input)))
                         (if (null (zstream-input-stack input))
                             (values :eof nil)
                           (values :S nil) ;fake #x20 after PE expansion
                           ))))
                 (t
                  (read-token-3 input)))))))

(defvar *data-behaviour*
    )           ;either :DTD or :DOC

(defun read-token-3 (zinput)
  (let ((input (car (zstream-input-stack zinput))))
    ;; PI Comment
    (let ((c (read-rune input)))
      (cond
       ;; first the common tokens
       ((rune= #/< c)
        (read-token-after-|<| zinput input))
       ;; now dispatch
       (t
        (ecase *data-behaviour*
          (:DTD
           (cond ((rune= #/\[ c) :\[)
                 ((rune= #/\] c) :\])
                 ((rune= #/\( c) :\()
                 ((rune= #/\) c) :\))
                 ((rune= #/\| c) :\|)
                 ((rune= #/\> c) :\>)
                 ((rune= #/\" c) :\")
                 ((rune= #/\' c) :\')
                 ((rune= #/\, c) :\,)
                 ((rune= #/\? c) :\?)
                 ((rune= #/\* c) :\*)
                 ((rune= #/\+ c) :\+)
                 ((name-rune-p c)
                  (unread-rune c input)
                  (values :name (read-name-token input)))
                 ((rune= #/# c)
                  (let ((q (read-name-token input)))
                    (cond ((equalp q '#.(string-rod "REQUIRED")) :|#REQUIRED|)
                          ((equalp q '#.(string-rod "IMPLIED")) :|#IMPLIED|)
                          ((equalp q '#.(string-rod "FIXED"))   :|#FIXED|)
                          ((equalp q '#.(string-rod "PCDATA"))  :|#PCDATA|)
                          (t
                           (error "Unknown token: ~S." q)))))
                 ((or (rune= c #/U+0020)
                      (rune= c #/U+0009)
                      (rune= c #/U+000D)
                      (rune= c #/U+000A))
                  (values :S nil))
                 ((rune= #/% c)
                  (cond ((name-start-rune-p (peek-rune input))
                         ;; an entity reference
                         (read-pe-reference zinput))
                        (t
                         (values :%))))
                 (t
                  (error "Unexpected character ~S." c))))
          (:DOC
           (cond 
            ((rune= c #/&)
             (multiple-value-bind (kind data) (read-entity-ref input)
               (cond ((eq kind :NAMED)
                      (values :ENTITY-REF data) )
                     ((eq kind :NUMERIC)
                      (values :CDATA
                              (with-rune-collector (collect)
                                (%put-unicode-char data collect)))))))
            (t
             (unread-rune c input)
             (values :CDATA (read-cdata input))) ))))))))

(defun read-pe-reference (zinput)
  (let* ((input (car (zstream-input-stack zinput)))
         (nam (read-name-token input)))
    (assert (rune= #/\; (read-rune input)))
    (cond (*expand-pe-p*
           ;; no external entities here!
           (let ((i2 (entity->xstream nam :parameter)))
             (zstream-push i2 zinput))
           (values :S nil) ;space before inserted PE expansion.
           )
          (t
           (values :PE-REFERENCE nam)) )))

(defun read-token-after-|<| (zinput input)
  (let ((d (read-rune input)))
    (cond ((eq d :eof)
           (error "EOF after '<'"))
          ((rune= #/! d)
           (read-token-after-|<!| input))
          ((rune= #/? d)
           (multiple-value-bind (target content) (read-pi input)
             (cond ((rod= target '#.(string-rod "xml"))
                    (values :xml-pi (cons target content)))
                   ((rod-equal target '#.(string-rod "XML"))
                    (error "You lost -- no XML processing instructions."))
		   ((and sax:*namespace-processing* (position #/: target))
		    (error "Processing instruction target ~S is not a valid NcName."
			   (mu target)))
                   (t
                    (values :PI (cons target content))))))
          ((rune= #// d)
           (let ((c (peek-rune input)))
             (cond ((name-start-rune-p c)
                    (read-tag-2 zinput input :etag))
                   (t
                    (error "Expecting name start rune after \"</\".")))))
          ((name-start-rune-p d)
           (unread-rune d input)
           (read-tag-2 zinput input :stag))
          (t
           (error "Expected '!' or '?' after '<' in DTD.")))))

(defun read-token-after-|<!| (input)
  (let ((d (read-rune input)))
    (cond ((eq d :eof)
           (error "EOF after \"<!\"."))
          ((name-start-rune-p d)
           (unread-rune d input)
           (let ((name (read-name-token input)))
             (cond ((rod= name '#.(string-rod "ELEMENT")) :|<!ELEMENT|)
                   ((rod= name '#.(string-rod "ENTITY")) :|<!ENTITY|)
                   ((rod= name '#.(string-rod "ATTLIST")) :|<!ATTLIST|)
                   ((rod= name '#.(string-rod "NOTATION")) :|<!NOTATION|)
                   ((rod= name '#.(string-rod "DOCTYPE")) :|<!DOCTYPE|)
                   (t
                    (error "`<!~A' unknown." (rod-string name))))))
          ((rune= #/\[ d)
           (values :|<![| nil))
          ((rune= #/- d)
           (setf d (read-rune input))
           (cond ((rune= #/- d)
                  (values
                   :COMMENT
                   (read-comment-content input)))
                 (t
                  (error "Bad character ~S after \"<!-\"" d))))
          (t
           (error "Bad character ~S after \"<!\"" d)))))

(defun read-attribute-list (zinput input imagine-space-p)
  (cond ((or imagine-space-p
             (let ((c (peek-rune input)))
               (and (not (eq c :eof))
                    (space-rune-p c))))
         (read-S? input)
         (cond ((eq (peek-rune input) :eof)
                nil)
               ((name-start-rune-p (peek-rune input))
                (cons (read-attribute zinput input)
                      (read-attribute-list zinput input nil)))
               (t
                nil)))
        (t
         nil)))

(defun read-entity-ref (input)
  "Read an entity reference off the xstream `input'. Returns two values:
   either :NAMED <interned-rod> in case of a named entity
   or     :NUMERIC <integer> in case of numeric entities.
   The initial #\\& is considered to be consumed already."
  (let ((c (peek-rune input)))
    (cond ((eq c :eof)
           (error "EOF after '&'"))
          ((rune= c #/#)
           (values :NUMERIC (read-numeric-entity input)))
          (t
           (unless (name-start-rune-p (peek-rune input))
             (error "Expecting name after &."))
           (let ((name (read-name-token input)))
             (setf c (read-rune input))
             (unless (rune= c #/\;)
               (perror input "Expected \";\"."))
             (values :NAMED name))))))

(defsubst read-S? (input)
  (while (member (peek-rune input) '(#/U+0020 #/U+0009 #/U+000A #/U+000D)
                 :test #'eq)
    (consume-rune input)))

(defun read-tag-2 (zinput input kind)
  (let ((name (read-name-token input))
        (atts nil))
    (setf atts (read-attribute-list zinput input nil))

    ;; check for double attributes
    (do ((q atts (cdr q)))
        ((null q))
      (cond ((find (caar q) (cdr q) :key #'car)
             (error "Attribute ~S has two definitions in element ~S."
                    (rod-string (caar q))
                    (rod-string name)))))

    (cond ((eq (peek-rune input) #/>)
           (consume-rune input)
           (values kind (cons name atts)))
          ((eq (peek-rune input) #//)
           (consume-rune input)
           (assert (rune= #/> (read-rune input)))
           (values :ztag (cons name atts)))
          (t
           (error "syntax error in read-tag-2.")) )))

(defun read-attribute (zinput input)
  (unless (name-start-rune-p (peek-rune input))
    (error "Expected name."))
  ;; arg thanks to the post mortem nature of name space declarations,
  ;; we could only process the attribute values post mortem.
  (let ((name (read-name-token input)))
    (while (let ((c (peek-rune input)))
             (and (not (eq c :eof))
                  (or (rune= c #/U+0020)
                      (rune= c #/U+0009)
                      (rune= c #/U+000A)
                      (rune= c #/U+000D))))
      (consume-rune input))
    (unless (eq (read-rune input) #/=)
      (perror zinput "Expected \"=\"."))
    (while (let ((c (peek-rune input)))
             (and (not (eq c :eof))
                  (or (rune= c #/U+0020)
                      (rune= c #/U+0009)
                      (rune= c #/U+000A)
                      (rune= c #/U+000D))))
      (consume-rune input))
    (cons name (read-att-value-2 input))
    ;;(cons name (read-att-value zinput input :ATT t))
    ))

(defun canon-not-cdata-attval (value)
  ;; | If the declared value is not CDATA, then the XML processor must
  ;; | further process the normalized attribute value by discarding any
  ;; | leading and trailing space (#x20) characters, and by replacing
  ;; | sequences of space (#x20) characters by a single space (#x20)
  ;; | character.
  (with-rune-collector (collect)
    (let ((gimme-20 nil)
          (anything-seen-p nil))
      (map nil (lambda (c)
                 (cond ((rune= c #/u+0020)
                        (setf gimme-20 t))
                       (t
                        (when (and anything-seen-p gimme-20)
                          (collect #/u+0020))
                        (setf gimme-20 nil)
                        (setf anything-seen-p t)
                        (collect c))))
           value))))

(defsubst data-rune-p (rune)
  ;; any Unicode character, excluding the surrogate blocks, FFFE, and FFFF.
  (let ((c (rune-code rune)))
    (or (= c #x9) (= c #xA) (= c #xD)
        (<= #x20 c #xD7FF)
        (<= #xE000 c #xFFFD)
        ;;
        (<= #xD800 c #xDBFF)
        (<= #xDC00 c #xDFFF)
        ;;
        )))

(defun read-att-value (zinput input mode &optional canon-space-p (delim nil))
  (with-rune-collector-2 (collect)
    (labels ((muffle (input delim)
               (let (c)
                 (loop
                   (setf c (read-rune input))
                   (cond ((eql delim c)
                          (return))
                         ((eq c :eof)
                          (error "EOF"))
                         ((rune= c #/&)
                          (setf c (peek-rune input))
                          (cond ((rune= c #/#)
                                 (let ((c (read-numeric-entity input)))
                                   (%put-unicode-char c collect)))
                                (t
                                 (unless (name-start-rune-p (peek-rune input))
                                   (error "Expecting name after &."))
                                 (let ((name (read-name-token input)))
                                   (setf c (read-rune input))
                                   (assert (rune= c #/\;))
                                   (ecase mode
                                     (:ATT
                                      (recurse-on-entity 
                                       zinput name :general
                                       (lambda (zinput)
                                         (muffle (car (zstream-input-stack zinput))
                                                 :eof))))
                                     (:ENT
                                      ;; bypass, but never the less we
                                      ;; need to check for legal
                                      ;; syntax.
                                      ;; Must it be defined?
                                      ;; allerdings: unparsed sind verboten
                                      (collect #/&)
                                      (map nil (lambda (x) (collect x)) name)
                                      (collect #/\; )))))))
                         ((and (eq mode :ENT) (rune= c #/%))
                          (unless (name-start-rune-p (peek-rune input))
                            (error "Expecting name after %."))
                          (let ((name (read-name-token input)))
                            (setf c (read-rune input))
                            (assert (rune= c #/\;))
                            (cond (*expand-pe-p*
                                   (recurse-on-entity 
                                    zinput name :parameter
                                    (lambda (zinput)
                                      (muffle (car (zstream-input-stack zinput))
                                              :eof))))
                                  (t
                                   (error "No PE here.")))))
                         ((and (eq mode :ATT) (rune= c #/<))
                          ;; xxx fix error message
                          (cerror "Eat them in spite of this."
                                  "For no apparent reason #\/< is forbidden in attribute values. ~
                           You lost -- next time choose SEXPR syntax.")
                          (collect c))
                         ((and canon-space-p (space-rune-p c))
                          (collect #/space))
                         ((not (data-rune-p c))
                          (error "illegal char: ~S." c))
                         (t
                          (collect c)))))))
      (declare (dynamic-extent #'muffle))
      (muffle input (or delim
                        (let ((delim (read-rune input)))
                          (assert (member delim '(#/\" #/\')))
                          delim))))))

(defun read-numeric-entity (input)
  ;; xxx eof handling
  ;; The #/& is already read
  (let ((res
         (let ((c (read-rune input)))
           (assert (rune= c #/#))
           (setq c (read-rune input))
           (cond ((rune= c #/x)
                  ;; hexadecimal
                  (setq c (read-rune input))
                  (assert (digit-rune-p c 16))
                  (prog1
                      (parse-integer
                       (with-output-to-string (sink)
                         (write-char (rune-char c) sink)
                         (while (digit-rune-p (setq c (read-rune input)) 16)
                           (write-char (rune-char c) sink)))
                       :radix 16)
                    (assert (rune= c #/\;)))
                  )
                 ((rune<= #/0 c #/9)
                  ;; decimal
                  (prog1
                      (parse-integer
                       (with-output-to-string (sink)
                         (write-char (rune-char c) sink)
                         (while (rune<= #/0 (setq c (read-rune input)) #/9)
                           (write-char (rune-char c) sink)))
                       :radix 10)
                    (assert (rune= c #/\;))) )
                 (t
                  (error "Bad char in numeric character entity.") )))))
    (unless (code-data-char-p res)
      (error "expansion of numeric character reference (#x~X) is no data char."
             res))
    res))

(defun read-pi (input)
  ;; "<?" is already read
  (let (name)
    (let ((c (peek-rune input)))
      (unless (name-start-rune-p c)
        (error "Expecting name after '<?'"))
      (setf name (read-name-token input)))
    (values name
            (read-pi-content input))))

(defun read-pi-content (input &aux d)
  (read-S? input)
  (with-rune-collector (collect)
    (block nil
      (tagbody
       state-1
        (setf d (read-rune input))
        (unless (data-rune-p d)
          (error "Illegal char: ~S." d))
        (when (rune= d #/?) (go state-2))
        (collect d)
        (go state-1)
       state-2 ;; #/? seen
        (setf d (read-rune input))
        (unless (data-rune-p d)
          (error "Illegal char: ~S." d))
        (when (rune= d #/>) (return))
        (when (rune= d #/?) 
          (collect #/?) 
          (go state-2))
        (collect #/?)
        (collect d)
        (go state-1)))))

(defun read-comment-content (input &aux d)
  (let ((warnedp nil))
    (with-rune-collector (collect)
      (block nil
        (tagbody
         state-1
          (setf d (read-rune input))
          (unless (data-rune-p d)
            (error "Illegal char: ~S." d))
          (when (rune= d #/-) (go state-2))
          (collect d)
          (go state-1)
         state-2 ;; #/- seen
          (setf d (read-rune input))
          (unless (data-rune-p d)
            (error "Illegal char: ~S." d))
          (when (rune= d #/-) (go state-3))
          (collect #/-)
          (collect d)
          (go state-1)
         state-3 ;; #/- #/- seen
          (setf d (read-rune input))
          (unless (data-rune-p d)
            (error "Illegal char: ~S." d))
          (when (rune= d #/>) (return))
          (unless warnedp
            (warn "WFC: no '--' in comments please.")
            (setf warnedp t))
          (when (rune= d #/-)
            (collect #/-)
            (go state-3))
          (collect #/-)
          (collect #/-)
          (collect d)
          (go state-1))))))

(defun read-cdata-sect (input &aux d)
  ;; <![CDATA[ is already read
  ;; read anything up to ]]>
  (with-rune-collector (collect)
    (block nil
      (tagbody
       state-1
        (setf d (read-rune input))
        (unless (data-rune-p d)
          (error "Illegal char: ~S." d))
        (when (rune= d #/\]) (go state-2))
        (collect d)
        (go state-1)
       state-2 ;; #/] seen
        (setf d (read-rune input))
        (unless (data-rune-p d)
          (error "Illegal char: ~S." d))
        (when (rune= d #/\]) (go state-3))
        (collect #/\])
        (collect d)
        (go state-1)
       state-3 ;; #/\] #/\] seen
        (setf d (read-rune input))
        (unless (data-rune-p d)
          (error "Illegal char: ~S." d))
        (when (rune= d #/>)
          (return))
        (when (rune= d #/\])
          (collect #/\])
          (go state-3))
        (collect #/\])
        (collect #/\])
        (collect d)
        (go state-1)))))

#+(or) ;; FIXME: There is another definition below that looks more reasonable.
(defun read-cdata (input initial-char &aux d)
  (cond ((not (data-rune-p initial-char))
         (error "Illegal char: ~S." initial-char)))
  (with-rune-collector (collect)
    (block nil
      (tagbody
        (cond ((rune= initial-char #/\])
               (go state-2))
              (t
               (collect initial-char)))
       state-1
        (setf d (peek-rune input))
        (when (or (eq d :eof) (rune= d #/<) (rune= d #/&))
          (return))
        (read-rune input)
        (unless (data-rune-p d)
          (error "Illegal char: ~S." d))
        (when (rune= d #/\]) (go state-2))
        (collect d)
        (go state-1)
        
       state-2 ;; #/\] seen
        (setf d (peek-rune input))
        (when (or (eq d :eof) (rune= d #/<) (rune= d #/&))
          (collect #/\])
          (return))
        (read-rune input)
        (unless (data-rune-p d)
          (error "Illegal char: ~S." d))
        (when (rune= d #/\]) (go state-3))
        (collect #/\])
        (collect d)
        (go state-1)
        
       state-3 ;; #/\] #/\] seen
        (setf d (peek-rune input))
        (when (or (eq d :eof) (rune= d #/<) (rune= d #/&))
          (collect #/\])
          (collect #/\])
          (return))
        (read-rune input)
        (unless (data-rune-p d)
          (error "Illegal char: ~S." d))
        (when (rune= d #/>) 
          (error "For no apparent reason ']]>' in not allowed in a CharData token -- you lost."))
        (when (rune= d #/\])
          (collect #/\])
          (go state-3))
        (collect #/\])
        (collect #/\])
        (collect d)
        (go state-1)))))


;; some character categories

(defun space-rune-p (rune)
  (declare (type rune rune))
  (or (rune= rune #/U+0020)
      (rune= rune #/U+0009)
      (rune= rune #/U+000A)
      (rune= rune #/U+000D)))

(defun code-data-char-p (c)
  ;; any Unicode character, excluding the surrogate blocks, FFFE, and FFFF.
  (or (= c #x9) (= c #xA) (= c #xD)
      (<= #x20 c #xD7FF)
      (<= #xE000 c #xFFFD)
      (<= #x10000 c #x10FFFF)))

(defun pubid-char-p (c)
  (or (rune= c #/u+0020) (rune= c #/u+000D) (rune= c #/u+000A)
      (rune<= #/a c #/z)
      (rune<= #/A c #/Z)
      (rune<= #/0 c #/9)
      (member c '(#/- #/' #/\( #/\) #/+ #/, #/. #//
                  #/: #/= #/? #/\; #/! #/* #/#
                  #/@ #/$ #/_ #/%))))


(defun expect (input category)
  (multiple-value-bind (cat sem) (read-token input)
    (unless (eq cat category)
      (error "Expected ~S saw ~S [~S]" category cat sem))
    (values cat sem)))

(defun consume-token (input)
  (read-token input))

;;;; ---------------------------------------------------------------------------
;;;;  Parser
;;;;

(defun p/S (input)
  ;; S ::= (#x20 | #x9 | #xD | #xA)+
  (expect input :S)
  (while (eq (peek-token input) :S)
    (consume-token input)))

(defun p/S? (input)
  ;; S ::= (#x20 | #x9 | #xD | #xA)+
  (while (eq (peek-token input) :S)
    (consume-token input)))

(defun p/name (input)
  (nth-value 1 (expect input :name)))

(defun p/attlist-decl (input)
  ;; [52] AttlistDecl ::= '<!ATTLIST' S Name (S AttDef)* S? '>'
  (let (elm-name)
    (expect input :|<!ATTLIST|)
    (p/S input)
    (setf elm-name (p/name input))
    (loop
      (let ((tok (read-token input)))
        (case tok
          (:S
           (p/S? input)
           (cond ((eq (peek-token input) :>)
                  (consume-token input)
                  (return))
                 (t
                  (multiple-value-bind (name type default) (p/attdef input)
                    (define-attribute (dtd *ctx*) elm-name name type default)) )))
          (:>
           (return))
          (otherwise
           (error "Expected either another AttDef or end of \"<!ATTLIST\". -- saw ~S."
                  tok)) )) )))

(defun p/attdef (input)
  ;; [53] AttDef ::= Name S AttType S DefaultDecl
  (let (name type default)
    (setf name (p/name input))
    (p/S input)
    (setf type (p/att-type input))
    (p/S input)
    (setf default (p/default-decl input))
    (values name type default)))

(defun p/list (input item-parser delimiter)
  ;; Parse something like S? <item> (S? <delimiter> <item>)* S?
  ;;
  (declare (type function item-parser))
  (let (res)
    (p/S? input)
    (setf res (list (funcall item-parser input)))
    (loop
      (p/S? input)
      (cond ((eq (peek-token input) delimiter)
             (consume-token input)
             (p/S? input)
             (push (funcall item-parser input) res))
            (t
             (return))))
    (p/S? input)
    (reverse res)))

(defun p/att-type (input)
  ;; [54] AttType ::= StringType | TokenizedType | EnumeratedType
  ;; [55] StringType ::= 'CDATA'
  ;; [56] TokenizedType ::= 'ID'                          /*VC: ID */
  ;;                                                        /*VC: One ID per Element Type */
  ;;                                                        /*VC: ID Attribute Default */
  ;;                          | 'IDREF'                     /*VC: IDREF */
  ;;                          | 'IDREFS'                    /*VC: IDREF */
  ;;                          | 'ENTITY'                    /*VC: Entity Name */
  ;;                          | 'ENTITIES'                  /*VC: Entity Name */
  ;;                          | 'NMTOKEN'                   /*VC: Name Token */
  ;;                          | 'NMTOKENS'                  /*VC: Name Token */
  ;; [57] EnumeratedType ::= NotationType | Enumeration
  ;; [58]   NotationType ::= 'NOTATION' S '(' S? Name (S? '|' S? Name)* S? ')'
  ;; /* VC: Notation Attributes */
  ;; [59] Enumeration ::= '(' S? Nmtoken (S? '|' S? Nmtoken)* S? ')' /* VC: Enumeration */
  (multiple-value-bind (cat sem) (read-token input)
    (cond ((eq cat :name)
           (cond ((equalp sem '#.(string-rod "CDATA"))    :CDATA)
                 ((equalp sem '#.(string-rod "ID"))       :ID)
                 ((equalp sem '#.(string-rod "IDREF"))    :IDREFS)
                 ((equalp sem '#.(string-rod "IDREFS"))   :IDREFS)
                 ((equalp sem '#.(string-rod "ENTITY"))   :ENTITY)
                 ((equalp sem '#.(string-rod "ENTITIES")) :ENTITIES)
                 ((equalp sem '#.(string-rod "NMTOKEN"))  :NMTOKEN)
                 ((equalp sem '#.(string-rod "NMTOKENS")) :NMTOKENS)
                 ((equalp sem '#.(string-rod "NOTATION"))
                  (let (names)
                    (p/S input)
                    (expect input :\()
                    (setf names (p/list input #'p/name :\| ))
                    (expect input :\))
                    (when *validate*
                      (setf (referenced-notations *ctx*)
                            (append names (referenced-notations *ctx*))))
                    (cons :NOTATION names)))
                 (t
                  (error "In p/att-type: ~S ~S." cat sem))))
          ((eq cat :\()
           ;; XXX Die Nmtoken-Syntax pruefen wir derzeit nur beim Validieren.
           (let (names)
             ;;(expect input :\()
             (setf names (p/list input #'p/name :\| ))
             (expect input :\))
             (cons :ENUMERATION names)))
          (t
           (error "In p/att-type: ~S ~S." cat sem)) )))

(defun p/default-decl (input)
  ;; [60] DefaultDecl ::= '#REQUIRED' | '#IMPLIED'
  ;;                       | (('#FIXED' S)? AttValue) /* VC: Required Attribute */
  ;;                       
  ;; /* VC: Attribute Default Legal */
  ;; /* WFC: No < in Attribute Values */
  ;; /* VC: Fixed Attribute Default */
  (multiple-value-bind (cat sem) (peek-token input)
    (cond ((eq cat :|#REQUIRED|) 
           (consume-token input) :REQUIRED)
          ((eq cat :|#IMPLIED|)  
           (consume-token input) :IMPLIED)
          ((eq cat :|#FIXED|)
           (consume-token input)
           (p/S input)
           (list :FIXED (p/att-value input)))
          ((or (eq cat :\') (eq cat :\"))
           (list :DEFAULT (p/att-value input)))
          (t
           (error "p/default-decl: ~S ~S." cat sem)) )))
;;;;

;;  [70] EntityDecl ::= GEDecl | PEDecl
;;  [71]     GEDecl ::= '<!ENTITY' S Name S EntityDef S? '>'
;;  [72]     PEDecl ::= '<!ENTITY' S '%' S Name S PEDef S? '>'
;;  [73]  EntityDef ::= EntityValue | (ExternalID NDataDecl?)
;;  [74]      PEDef ::= EntityValue | ExternalID
;;  [75] ExternalID ::= 'SYSTEM' S SystemLiteral
;;                      | 'PUBLIC' S PubidLiteral S SystemLiteral
;;  [76]  NDataDecl ::= S 'NDATA' S Name                /* VC: Notation Declared */

(defun p/entity-decl (input)
  (let (name def kind)
    (expect input :|<!ENTITY|)
    (p/S input)
    (cond ((eq (peek-token input) :%)
           (setf kind :parameter)
           (consume-token input)
           (p/S input))
          (t
           (setf kind :general)))
    (setf name (p/name input))
    (p/S input)
    (setf def (p/entity-def input kind))
    (define-entity input name kind def)
    (when (eq kind :general)
      ;; XXX wirklich nur :general?
      ;; XXX ist sax:unparsed-entity-declaration ueberhaupt richtig?
      ;; Brauchen wir nicht sax:internal- und sax:external-entity-declaration?
      (ecase (car def)
        (:EXTERNAL
          (destructuring-bind (id notation) (cdr def)
            (ecase (car id)
              (:PUBLIC
                (sax:unparsed-entity-declaration
                 (handler *ctx*) name (second id) (third id) notation))
              (:SYSTEM
                (sax:unparsed-entity-declaration
                 (handler *ctx*) name nil (second id) notation)))))
        (:INTERNAL
          (sax:unparsed-entity-declaration (handler *ctx*) name nil nil nil))))
    (p/S? input)
    (expect input :\>)))

(defun p/entity-def (input kind)
  (multiple-value-bind (cat sem) (peek-token input)
    (cond ((member cat '(:\" :\'))
           (list :INTERNAL (p/entity-value input)))
          ((and (eq cat :name)
                (or (equalp sem '#.(string-rod "SYSTEM"))
                    (equalp sem '#.(string-rod "PUBLIC"))))
           (let (extid ndata)
             (setf extid (p/external-id input nil))
             (when (eq kind :general)   ;NDATA allowed at all?
               (cond ((eq (peek-token input) :S)
                      (p/S? input)
                      (when (and (eq (peek-token input) :name)
                                 (equalp (nth-value 1 (peek-token input))
                                         '#.(string-rod "NDATA")))
                        (consume-token input)
                        (p/S input)
                        (setf ndata (p/name input))
                        (when *validate*
                          (push ndata (referenced-notations *ctx*)))))))
             (list :EXTERNAL extid ndata)))
          (t
           (error "p/entity-def: ~S / ~S." cat sem)) )))

(defun p/entity-value (input)
  (let ((delim (if (eq (read-token input) :\") #/\" #/\')))
    (read-att-value input
                    (car (zstream-input-stack input))
                    :ENT
                    nil
                    delim)))

(defun p/att-value (input)
  (let ((delim (if (eq (read-token input) :\") #/\" #/\')))
    (read-att-value input
                    (car (zstream-input-stack input))
                    :ATT
                    t
                    delim)))

(defun p/external-id (input &optional (public-only-ok-p nil))
  ;; xxx public-only-ok-p
  (multiple-value-bind (cat sem) (read-token input)
    (cond ((and (eq cat :name) (equalp sem '#.(string-rod "SYSTEM")))
           (p/S input)
           (list :SYSTEM (p/system-literal input))
           )
          ((and (eq cat :name) (equalp sem '#.(string-rod "PUBLIC")))
           (let (pub sys)
             (p/S input)
             (setf pub (p/pubid-literal input))
             (when (eq (peek-token input) :S)
               (p/S input)
               (when (member (peek-token input) '(:\" :\'))
                 (setf sys (p/system-literal input))))
             (unless (every #'pubid-char-p pub)
               (error "Illegal pubid: ~S." (rod-string pub)))
             (when (and (not public-only-ok-p)
                        (null sys))
               (error "System identifier needed for this PUBLIC external identifier."))
             (list :PUBLIC pub sys)))
          (t
           (error "Expected external-id: ~S / ~S." cat sem)))))


;;  [11] SystemLiteral ::= ('"' [^"]* '"') | ("'" [^']* "'")
;;  [12]  PubidLiteral ::= '"' PubidChar* '"' | "'" (PubidChar - "'")* "'"
;;  [13]     PubidChar ::= #x20 | #xD | #xA | [a-zA-Z0-9]
;;                         | [-'()+,./:=?;!*#@$_%]

(defun p/system-literal (input)
  (multiple-value-bind (cat) (read-token input)
    (cond ((member cat '(:\" :\'))
           (let ((delim (if (eq cat :\") #/\" #/\')))
             (with-rune-collector (collect)
               (loop
                 (let ((c (read-rune (car (zstream-input-stack input)))))
                   (cond ((eq c :eof)
                          (error "EOF in system literal."))
                         ((rune= c delim)
                          (return))
                         (t
                          (collect c))))))))
          (t
           (error "Expect either \" or \'.")))))

(defun p/pubid-literal (input)
  ;; xxx check for valid chars
  (p/system-literal input))


;;;;

(defun p/element-decl (input)
  (let (name content)
    (expect input :|<!ELEMENT|)
    (p/S input)
    (setf name (p/name input))
    (p/S input)
    (setf content (normalize-mixed-cspec (p/cspec input)))
    (unless (legal-content-model-p content *validate*)
      (error "Malformed or invalid content model: ~S." (mu content)))
    (p/S? input)
    (expect input :\>)
    (when *validate*
      (define-element (dtd *ctx*) name content))
    (list :element name content)))

(defun maybe-compile-cspec (e)
  (or (elmdef-compiled-cspec e)
      (setf (elmdef-compiled-cspec e)
            (let ((cspec (elmdef-content e)))
              (unless cspec
                (validity-error "(03) Element Valid: no definition for ~A"
                                (rod-string (elmdef-name e))))
              (multiple-value-call #'cons
                (compile-cspec cspec (standalone-check-necessary-p e)))))))

(defun make-root-model (name)
  (cons (lambda (actual-name)
          (if (rod= actual-name name)
              (constantly :dummy)
              nil))
        (constantly t)))

;;; content spec validation:
;;;
;;; Given a `contentspec', COMPILE-CSPEC returns as multiple values two
;;; functions A and B of one argument to be called for every
;;;   A. child element
;;;   B. text child node
;;;
;;; Function A will be called with
;;;   - the element name rod as its argument.  If that element may appear
;;;     at the current position, a new function to be called for the next
;;;     child is returned.  Otherwise NIL is returned.
;;;   - argument NIL at the end of the element, it must then return T or NIL
;;;     to indicate whether the end tag is valid.
;;;
;;; Function B will be called with the character data rod as its argument, it
;;; returns a boolean indicating whether this text element is allowed.
;;;
;;; That is, if one of the functions ever returns NIL, the element is
;;; rejected as invalid.

(defun cmodel-done (actual-value)
  (null actual-value))

(defun compile-cspec (cspec &optional standalone-check)
  (cond
    ((atom cspec)
      (ecase cspec
        (:EMPTY (values #'cmodel-done (constantly nil)))
        (:PCDATA (values #'cmodel-done (constantly t)))
        (:ANY
          (values (labels ((doit (name) (if name #'doit t))) #'doit)
                  (constantly t)))))
    ((and (eq (car cspec) '*)
          (let ((subspec (second cspec)))
            (and (eq (car subspec) 'or) (eq (cadr subspec) :PCDATA))))
      (values (compile-mixed (second cspec))
              (constantly t)))
    (t
      (values (compile-content-model cspec)
              (lambda (rod)
                (when standalone-check
                  (validity-error "(02) Standalone Document Declaration: whitespace"))
                (every #'white-space-rune-p rod))))))

(defun compile-mixed (cspec)
  ;; das koennten wir theoretisch auch COMPILE-CONTENT-MODEL erledigen lassen
  (let ((allowed-names (cddr cspec)))
    (labels ((doit (actual-name)
               (cond
                 ((null actual-name) t)
                 ((member actual-name allowed-names :test #'rod=) #'doit)
                 (t nil))))
      #'doit)))

(defun compile-content-model (cspec &optional (continuation #'cmodel-done))
  (if (vectorp cspec)
      (lambda (actual-name)
        (if (and actual-name (rod= cspec actual-name))
            continuation
            nil))
      (ecase (car cspec)
        (and
          (labels ((traverse (seq)
                     (compile-content-model (car seq)
                                            (if (cdr seq)
                                                (traverse (cdr seq))
                                                continuation))))
            (traverse (cdr cspec))))
        (or
          (let ((options (mapcar (rcurry #'compile-content-model continuation)
                                 (cdr cspec))))
            (lambda (actual-name)
              (some (rcurry #'funcall actual-name) options))))
        (?
          (let ((maybe (compile-content-model (second cspec) continuation)))
            (lambda (actual-name)
              (or (funcall maybe actual-name)
                  (funcall continuation actual-name)))))
        (*
          (let (maybe-continuation)
            (labels ((recurse (actual-name)
                       (if (null actual-name)
                           (funcall continuation actual-name)
                           (or (funcall maybe-continuation actual-name)
                               (funcall continuation actual-name)))))
              (setf maybe-continuation
                    (compile-content-model (second cspec) #'recurse))
              #'recurse)))
        (+
          (let ((it (cadr cspec)))
            (compile-content-model `(and ,it (* ,it)) continuation))))))

(defun setp (list &key (test 'eql))
  (equal list (remove-duplicates list :test test)))

(defun legal-content-model-p (cspec &optional validate)
  (or (eq cspec :PCDATA)
      (eq cspec :ANY)
      (eq cspec :EMPTY)
      (and (consp cspec)
           (eq (car cspec) '*)
           (consp (cadr cspec))
           (eq (car (cadr cspec)) 'or)
           (eq (cadr (cadr cspec)) :PCDATA)
           (every #'vectorp (cddr (cadr cspec)))
           (if (and validate (not (setp (cddr (cadr cspec)) :test #'rod=)))
               (validity-error "VC: No Duplicate Types (07)")
               t))
      (labels ((walk (x)
                 (cond ((member x '(:PCDATA :ANY :EMPTY))
                        nil)
                       ((atom x) t)
                       ((and (walk (car x))
                             (walk (cdr x)))))))
        (walk cspec))))
                 
;; wir fahren besser, wenn wir machen:

;; cspec ::= 'EMPTY' | 'ANY' | '#PCDATA' 
;;         | Name
;;         | cs
;;    cs ::= '(' S? cspec ( S? '|' S? cspec)* S? ')' ('?' | '*' | '+')?
;; und eine post mortem analyse

(defun p/cspec (input)
  (let ((term
         (let ((names nil) op-cat op res stream)
           (multiple-value-bind (cat sem) (peek-token input)
             (cond ((eq cat :name) 
                    (consume-token input) 
                    (cond ((rod= sem '#.(string-rod "EMPTY"))
                           :EMPTY)
                          ((rod= sem '#.(string-rod "ANY"))
                           :ANY)
                          (t
                           sem)))
                   ((eq cat :\#PCDATA)
                    (consume-token input)
                    :PCDATA)
                   ((eq cat :\()
                    (setf stream (car (zstream-input-stack input)))
                    (consume-token input)
                    (p/S? input)
                    (setq names (list (p/cspec input)))
                    (p/S? input)
                    (cond ((member (peek-token input) '(:\| :\,))
                            (setf op-cat (peek-token input))
                            (setf op (if (eq op-cat :\,) 'and 'or))
                            (while (eq (peek-token input) op-cat)
                              (consume-token input)
                              (p/S? input)
                              (push (p/cspec input) names)
                              (p/S? input))
                            (setf res (cons op (reverse names))))
                      (t
                        (setf res (cons 'and names))))
                    (p/S? input)
                    (expect input :\))
                    (when *validate*
                      (unless (eq stream (car (zstream-input-stack input)))
                        (validity-error "(06) Proper Group/PE Nesting")))
                    res)
                   (t
                    (error "p/cspec - ~s / ~s" cat sem)))))))
    (cond ((eq (peek-token input) :?) (consume-token input) (list '? term))
          ((eq (peek-token input) :+) (consume-token input) (list '+ term))
          ((eq (peek-token input) :*) (consume-token input) (list '* term))
          (t
           term))))

(defun normalize-mixed-cspec (cspec)
  ;; der Parser oben funktioniert huebsch fuer die children-Regel, aber
  ;; fuer Mixed ist das Ergebnis nicht praktisch, denn dort wollen wir
  ;; eigentlich auf eine Liste von Namen in einheitlichem Format hinaus.
  ;; Dazu normalisieren wir einfach in eine der beiden folgenden Formen:
  ;;   (* (or :PCDATA ...rods...))     -- und zwar exakt so!
  ;;   :PCDATA                         -- sonst ganz trivial
  (flet ((trivialp (c)
           (and (consp c)
                (and (eq (car c) 'and)
                     (eq (cadr c) :PCDATA)
                     (null (cddr c))))))
    (if (or (trivialp cspec)            ;(and PCDATA)
            (and (consp cspec)          ;(* (and PCDATA))
                 (and (eq (car cspec) '*)
                      (null (cddr cspec))
                      (trivialp (cadr cspec)))))
        :PCDATA
        cspec)))
   
;; [52] AttlistDecl ::= '<!ATTLIST' S Name AttDef* S? '>'
    

;; [52] AttlistDecl ::= '<!ATTLIST' S Name AttDefs S? '>'
;; [52] AttlistDecl ::= '<!ATTLIST' S Name S? '>'
;; [53] AttDefs ::= S Name S AttType S DefaultDecl AttDefs
;; [53] AttDefs ::= 

(defun p/notation-decl (input)
  (let (name id)
    (expect input :|<!NOTATION|)
    (p/S input)
    (setf name (p/name input))
    (p/S input)
    (setf id (p/external-id input t))
    (p/S? input)
    (expect input :\>)
    (case (car id)
      ;; XXX are these right?
      (:SYSTEM
        (sax:notation-declaration (handler *ctx*) name nil (cadr id)))
      (:PUBLIC
        (sax:notation-declaration (handler *ctx*) name (normalize-public-id (cadr id)) (caddr id))))
    (when *validate*
      (define-notation (dtd *ctx*) name id))
    (list :notation-decl name id)))

(defun normalize-public-id (rod)
  (with-rune-collector (collect)
    (let ((gimme-20 nil)
          (anything-seen-p nil))
      (map nil (lambda (c)
                 (cond
                   ((or (rune= c #/u+0009)
                        (rune= c #/u+000A)
                        (rune= c #/u+000D)
                        (rune= c #/u+0020))
                     (setf gimme-20 t))
                   (t
                     (when (and anything-seen-p gimme-20)
                       (collect #/u+0020))
                     (setf gimme-20 nil)
                     (setf anything-seen-p t)
                     (collect c))))
           rod))))

;;;

(defun p/conditional-sect (input)
  (expect input :<!\[ )
  (let ((stream (car (zstream-input-stack input))))
    (p/S? input)
    (multiple-value-bind (cat sem) (read-token input)
      (cond ((and (eq cat :name)
                  (rod= sem '#.(string-rod "INCLUDE")))
             (p/include-sect input stream))
            ((and (eq cat :name)
                  (rod= sem '#.(string-rod "IGNORE")))
             (p/ignore-sect input stream))
            (t
             (error "Expected INCLUDE or IGNORE after \"<![\"."))))))

(defun p/cond-expect (input cat initial-stream)
  (expect input cat)
  (when *validate*
    (unless (eq (car (zstream-input-stack input)) initial-stream)
      (validity-error "(21) Proper Conditional Section/PE Nesting"))))

(defun p/include-sect (input initial-stream)
  ;; <![INCLUDE is already read.
  (p/S? input)
  (p/cond-expect input :\[ initial-stream)
  (p/ext-subset-decl input)
  (p/cond-expect input :\] initial-stream)
  (p/cond-expect input :\] initial-stream)
  (p/cond-expect input :\> initial-stream))

(defun p/ignore-sect (input initial-stream)
  ;; <![IGNORE is already read.
  ;; XXX Is VC 21 being checked for nested sections?
  (p/S? input)
  (p/cond-expect input :\[ initial-stream)
  (let ((input (car (zstream-input-stack input))))
    (let ((level 0))
      (do ((c1 (read-rune input) (read-rune input))
           (c2 0 c1)
           (c3 0 c2))
          ((= level -1))
        (declare (type fixnum level))
        (cond ((eq c1 :eof)
               (error "EOF in <![IGNORE ... >")))
        (cond ((and (rune= c3 #/<) (rune= c2 #/!) (rune= c1 #/\[))
               (incf level)))
        (cond ((and (rune= c3 #/\]) (rune= c2 #/\]) (rune= c1 #/>))
               (decf level))) )))
  (unless (eq (car (zstream-input-stack input)) initial-stream)
    (validity-error "(21) Proper Conditional Section/PE Nesting")))

(defun p/ext-subset-decl (input)
  ;; ( markupdecl | conditionalSect | S )*
  (loop
    (case (let ((*expand-pe-p* nil)) (peek-token input))
      (:|<![| (let ((*expand-pe-p* t)) (p/conditional-sect input)))
      (:S     (consume-token input))
      (:eof   (return))
      ((:|<!ELEMENT| :|<!ATTLIST| :|<!ENTITY| :|<!NOTATION| :PI :COMMENT)
       (let ((*expand-pe-p* t)
             (*markup-declaration-external-p* t))
         (p/markup-decl input)))
      ((:PE-REFERENCE)
       (let ((name (nth-value 1 (read-token input))))
         (recurse-on-entity input name :parameter
                            (lambda (input)
                              (ecase (entity-source-kind name :parameter)
                                (:EXTERNAL
                                 (p/ext-subset input))
                                (:INTERNAL
                                 (p/ext-subset-decl input)))
                              (unless (eq :eof (peek-token input))
                                (error "Trailing garbage."))))))
      (otherwise (return)))) )

(defun p/markup-decl (input)
  (peek-token input)
  (let ((stream (car (zstream-input-stack input))))
    (multiple-value-prog1
        (p/markup-decl-unsafe input)
      (when *validate*
        (unless (eq stream (car (zstream-input-stack input)))
          (validity-error "(01) Proper Declaration/PE Nesting"))))))

(defun p/markup-decl-unsafe (input)
  ;; markupdecl ::= elementdecl | AttlistDecl       /* VC: Proper Declaration/PE Nesting */
  ;;              | EntityDecl | NotationDecl 
  ;;              | PI | Comment               /* WFC: PEs in Internal Subset */
  (case (peek-token input)
    (:|<!ELEMENT|  (p/element-decl input))
    (:|<!ATTLIST|  (p/attlist-decl input))
    (:|<!ENTITY|   (p/entity-decl input))
    (:|<!NOTATION| (p/notation-decl input))
    (:PI
      (let ((sem (nth-value 1 (read-token input))))
        (sax:processing-instruction (handler *ctx*) (car sem) (cdr sem))))
    (:COMMENT      (consume-token input))
    (otherwise
     (error "p/markup-decl ~S" (peek-token input)))))

(defun setup-encoding (input xml-header)
  (when (xml-header-encoding xml-header)
    (let ((enc (find-encoding (xml-header-encoding xml-header))))
      (cond (enc
             (setf (xstream-encoding (car (zstream-input-stack input)))
               enc))
            (t
             (warn "There is no such encoding: ~S." (xml-header-encoding xml-header)))))))

(defun set-full-speed (input)
  (let ((xstream (car (zstream-input-stack input))))
    (when xstream
      (set-to-full-speed xstream))))

(defun p/ext-subset (input)
  (cond ((eq (peek-token input) :xml-pi)
         (let ((hd (parse-xml-pi (cdr (nth-value 1 (peek-token input))) nil)))
           (setup-encoding input hd))
         (consume-token input)))
  (set-full-speed input)
  (p/ext-subset-decl input)
  (unless (eq (peek-token input) :eof)
    (error "Trailing garbage - ~S." (peek-token input))))

(defun p/doctype-decl (input)
  (let ((*expand-pe-p* nil))
    (let (name extid)
      (expect input :|<!DOCTYPE|)
      (p/S input)
      (setq name (p/name input))
      (when *validate*
        (setf (model-stack *ctx*) (list (make-root-model name))))
      (when (eq (peek-token input) :S)
        (p/S input)
        (unless (or (eq (peek-token input) :\[ )
                    (eq (peek-token input) :\> ))
          (setf extid (p/external-id input t))))
      (p/S? input)
      (ecase (car extid)
        (:PUBLIC (sax:start-dtd (handler *ctx*) name (cadr extid) (caddr extid)))
        (:SYSTEM (sax:start-dtd (handler *ctx*) name nil (cadr extid)))
        ((nil) (sax:start-dtd (handler *ctx*) name nil nil)))
      (when (eq (peek-token input) :\[ )
        (consume-token input)
        (while (progn (p/S? input)
                      (not (eq (peek-token input) :\] )))
          (if (eq (peek-token input) :PE-REFERENCE)
              (let ((name (nth-value 1 (read-token input))))
                (recurse-on-entity input name :parameter
                                   (lambda (input)
                                     (ecase (entity-source-kind name :parameter)
                                       (:EXTERNAL
                                        (p/ext-subset input))
                                       (:INTERNAL
                                        (p/ext-subset-decl input)))
                                     (unless (eq :eof (peek-token input))
                                       (error "Trailing garbage.")))))
            (let ((*expand-pe-p* t))
              (p/markup-decl input))))
        (consume-token input)
        (p/S? input))
      (expect input :>)
      (when extid
        ;; can we make this conditional on *validate*?
        ;; (What about entity references then?)
        (let* ((xi2 (open-extid (absolute-extid input extid)))
               (zi2 (make-zstream :input-stack (list xi2))))
          (p/ext-subset zi2)))
      (sax:end-dtd (handler *ctx*))
      (let ((dtd (dtd *ctx*)))
        (sax:entity-resolver
         (handler *ctx*)
         (lambda (name handler) (resolve-entity name handler dtd))))
      (list :DOCTYPE name extid))))

(defun p/misc*-2 (input)
  ;; Misc*
  (while (member (peek-token input) '(:COMMENT :PI :S))
    (case (peek-token input)
      (:COMMENT
        (sax:comment (handler *ctx*) (nth-value 1 (peek-token input))))
      (:PI
        (sax:processing-instruction 
         (handler *ctx*)
         (car (nth-value 1 (peek-token input)))
         (cdr (nth-value 1 (peek-token input))))))
    (consume-token input)))
  
;; forward declaration for DEFVAR
(declaim (special *default-namespace-bindings*))

(defun p/document (input handler &key validate root)
  (let ((*ctx* (make-context :handler handler))
        (*validate* (and validate t)))
    (define-default-entities)
    (sax:start-document handler)
    ;; document ::= XMLDecl? Misc* (doctypedecl Misc*)? element Misc*
    ;; Misc ::= Comment | PI |  S
    ;; xmldecl::='<?xml' VersionInfo EncodingDecl? SDDecl? S? '?>'
    ;; sddecl::= S 'standalone' Eq (("'" ('yes' | 'no') "'") | ('"' ('yes' | 'no') '"'))
    ;;
    ;; we will use the attribute-value parser for the xml decl.
    (let ((*data-behaviour* :DTD))
      ;; optional XMLDecl?
      (cond ((eq (peek-token input) :xml-pi)
             (let ((hd (parse-xml-pi (cdr (nth-value 1 (peek-token input))) t)))
               (setf (standalone-p *ctx*) (eq (xml-header-standalone-p hd) :yes))
               (setup-encoding input hd))
             (read-token input)))
      (set-full-speed input)
      ;; Misc*
      (p/misc*-2 input)
      ;; (doctypedecl Misc*)?
      (cond
        ((eq (peek-token input) :<!DOCTYPE)
          (p/doctype-decl input)
          (p/misc*-2 input))
        ((and validate (not (dtd-p validate)))
          (validity-error "invalid document: no doctype")))
      ;; Switch to caller-supplied DTD if asked to
      (when (dtd-p validate)
        (setf (dtd *ctx*) validate)
        (setf (model-stack *ctx*)
              (list (cons (lambda (x) x (constantly :dummy)) (constantly t)))))
      (when root
        (setf (model-stack *ctx*) (list (make-root-model root))))
      ;; element
      (let ((*data-behaviour* :DOC))
        (p/element input))
      ;; optional Misc*
      (p/misc*-2 input)
      (unless (eq (peek-token input) :eof)
        (error "Garbage at end of document."))
      (when *validate*
        (maphash (lambda (k v)
                   (unless v
                     (validity-error "(11) IDREF: ~S not defined" (rod-string k))))
                 (id-table *ctx*))
      
        (dolist (name (referenced-notations *ctx*)) 
          (unless (find-notation name (dtd *ctx*))
            (validity-error "(23) Notation Declared: ~S" (rod-string name))))) 
      (sax:end-document handler))))

(defun p/element (input)
  (if sax:*namespace-processing*
      (p/element-ns input)
      (p/element-no-ns input)))
    
(defun p/element-no-ns (input)
  ;;    [39] element ::= EmptyElemTag | STag content ETag
  (error "sorry, bitrot")
  (multiple-value-bind (cat sem) (read-token input)
    (cond ((eq cat :ztag)
	   (sax:start-element (handler *ctx*) nil nil (car sem) (build-attribute-list-no-ns (cdr sem)))
	   (sax:end-element (handler *ctx*) nil nil (car sem)))

          ((eq cat :stag)
	   (sax:start-element (handler *ctx*) nil nil (car sem) (build-attribute-list-no-ns (cdr sem)))
	   (p/content input)
	   (multiple-value-bind (cat2 sem2) (read-token input)
               (unless (and (eq cat2 :etag)
                            (eq (car sem2) (car sem)))
                 (perror input "Bad nesting. ~S / ~S" (mu sem) (mu (cons cat2 sem2)))))
	   (sax:end-element (handler *ctx*) nil nil (car sem)))

          (t
           (error "Expecting element.")))))


(defun p/element-ns (input)
  (destructuring-bind (cat (name &rest attrs))
      (multiple-value-list (read-token input))
    (validate-start-element *ctx* name)
    (let ((ns-decls (declare-namespaces name attrs)))
      (multiple-value-bind (ns-uri prefix local-name) (decode-qname name)
	(declare (ignore prefix))
	(let* ((raw-attlist (build-attribute-list-ns attrs))
               (attlist
                (remove-if-not (lambda (a)
                                 (or sax:*include-xmlns-attributes*
                                     (not (xmlns-attr-p (attribute-qname a)))))
                               (process-attributes *ctx* name raw-attlist))))
          (cond ((eq cat :ztag)
		 (sax:start-element (handler *ctx*) ns-uri local-name name attlist)
		 (sax:end-element (handler *ctx*) ns-uri local-name name))
		
		((eq cat :stag)
		 (sax:start-element (handler *ctx*) ns-uri local-name name attlist)
		 (p/content input)
		 (multiple-value-bind (cat2 sem2) (read-token input)
		   (unless (and (eq cat2 :etag)
				(eq (car sem2) name))
		     (perror input "Bad nesting. ~S / ~S" (mu name) (mu (cons cat2 sem2)))))
		 (sax:end-element (handler *ctx*) ns-uri local-name name))
		
		(t
		 (error "Expecting element, got ~S." cat)))))
      (undeclare-namespaces ns-decls))
    (validate-end-element *ctx* name)))
      
(defun perror (stream format-string &rest format-args)
  (when (zstream-p stream)
    (setf stream (car (zstream-input-stack stream))))
  (error "Parse error at line ~D column ~D: ~A" 
         (xstream-line-number stream)
         (xstream-column-number stream)
         (apply #'format nil format-string format-args)))

(defun p/content (input)
  ;; [43] content ::= (element | CharData | Reference | CDSect | PI | Comment)*
  (multiple-value-bind (cat sem) (peek-token input)
    (case cat
      ((:stag :ztag)
       (p/element input)
       (p/content input))
      ((:CDATA)
       (consume-token input)
       (validate-characters *ctx* sem)
       (sax:characters (handler *ctx*) sem)
       (p/content input))
      ((:ENTITY-REF)
       (let ((name sem))
         (consume-token input)
         (append ;; nil  #+(OR)
          (recurse-on-entity input name :general
                             (lambda (input)
                               (prog1
                                   (ecase (entity-source-kind name :general)
                                     (:INTERNAL (p/content input))
                                     (:EXTERNAL (p/ext-parsed-ent input)))
                                 (unless (eq (peek-token input) :eof)
                                   (error "Trailing garbage. - ~S" (peek-token input))))))
          (p/content input))))
      ((:<!\[)
       (consume-token input)
       (cons 
        (let ((input (car (zstream-input-stack input))))
          (unless (and (rune= #/C (read-rune input))
                       (rune= #/D (read-rune input))
                       (rune= #/A (read-rune input))
                       (rune= #/T (read-rune input))
                       (rune= #/A (read-rune input))
                       (rune= #/\[ (read-rune input)))
            (error "After '<![', 'CDATA[' is expected."))
	  (validate-characters *ctx* #"hack") ;anything other than whitespace
	  (sax:start-cdata (handler *ctx*))
	  (sax:characters (handler *ctx*) (read-cdata-sect input))
	  (sax:end-cdata (handler *ctx*)))
        (p/content input)))
      ((:PI)
       (consume-token input)
       (sax:processing-instruction (handler *ctx*) (car sem) (cdr sem))
       (p/content input))
      ((:COMMENT)
       (consume-token input)
       (sax:comment (handler *ctx*) sem)
       (p/content input))
      (otherwise
       nil))))

;; [78] extParsedEnt ::= TextDecl? contentw
;; [79]        extPE ::= TextDecl? extSubsetDecl

(defstruct xml-header
  version
  encoding
  (standalone-p nil))

(defun p/ext-parsed-ent (input)
  ;; [78] extParsedEnt ::= '<?xml' VersionInfo? EncodingDecl S? '?>' content
  (when (eq (peek-token input) :xml-pi)
    (let ((hd (parse-xml-pi (cdr (nth-value 1 (peek-token input))) nil)))
      (setup-encoding input hd))
    (consume-token input) )
  (set-full-speed input)
  (p/content input))

(defun parse-xml-pi (content sd-ok-p)
  ;; --> xml-header
  ;;(make-xml-header))
  (let* ((res (make-xml-header))
         (i (make-rod-xstream content))
         (atts (read-attribute-list 'foo i t))) ;xxx on 'foo
    (unless (eq (peek-rune i) :eof)
      (error "Garbage at end of XML PI."))
    ;; versioninfo muss da sein
    ;; dann ? encodingdecl 
    ;; dann ? sddecl
    ;; dann ende
    (when (and (not (eq (caar atts) (intern-name '#.(string-rod "version"))))
               sd-ok-p)
      (error "XML PI needs version."))
    (when (eq (caar atts) (intern-name '#.(string-rod "version")))
      (unless (and (>= (length (cdar atts)) 1)
                   (every (lambda (x)
                            (or (rune<= #/a x #/z)
                                (rune<= #/A x #/Z)
                                (rune<= #/0 x #/9)
                                (rune= x #/_)
                                (rune= x #/.)
                                (rune= x #/:)
                                (rune= x #/-)))
                          (cdar atts)))
        (error "Bad XML version number: ~S." (rod-string (cdar atts))))
      (setf (xml-header-version res) (rod-string (cdar atts)))
      (pop atts))
    (when (eq (caar atts) (intern-name '#.(string-rod "encoding")))
      (unless (and (>= (length (cdar atts)) 1)
                   (every (lambda (x)
                            (or (rune<= #/a x #/z)
                                (rune<= #/A x #/Z)
                                (rune<= #/0 x #/9)
                                (rune= x #/_)
                                (rune= x #/.)
                                (rune= x #/-)))
                          (cdar atts))
                   ((lambda (x)
                      (or (rune<= #/a x #/z)
                          (rune<= #/A x #/Z)
                          (rune<= #/0 x #/9)))
                    (aref (cdar atts) 0)))
        (error "Bad XML encoding name: ~S." (rod-string (cdar atts))))
      (setf (xml-header-encoding res) (rod-string (cdar atts)))
      (pop atts))
    (when (and sd-ok-p (eq (caar atts) (intern-name '#.(string-rod "standalone"))))
      (unless (or (rod= (cdar atts) '#.(string-rod "yes"))
                  (rod= (cdar atts) '#.(string-rod "no")))
        (error "Hypersensitivity pitfall: ~
                XML PI's 'standalone' attribute must be exactly \"yes\" or \"no\" and not ~S."
               (rod-string (cdar atts))))
      (setf (xml-header-standalone-p res) 
        (if (rod-equal '#.(string-rod "yes") (cdar atts))
            :yes
          :no))
      (pop atts))
    (when atts
      (error "XML designers decided to disallow future extensions to the set ~
              of allowed XML PI's attributes -- you might have lost big on ~S (~S)"
             (rod-string content) sd-ok-p
             )) 
    res))

;;;; ---------------------------------------------------------------------------
;;;;  mu
;;;;

(defun mu (x)
  (cond ((stringp x) x)
        ((vectorp x) (rod-string x))
        ((consp x)
         (cons (mu (car x)) (mu (cdr x))))
        (x)))

;;;; ---------------------------------------------------------------------------
;;;; User inteface ;;;;

(defun parse-file (filename handler &rest args)
  (with-open-xstream (input filename)
    (setf (xstream-name input)
      (make-stream-name
       :entity-name "main document"
       :entity-kind :main
       :file-name filename))
    (let ((zstream (make-zstream :input-stack (list input))))
      (peek-rune input)
      (progn 'time
       (apply #'p/document zstream handler args)))))

(defun parse-stream (stream handler &rest args)
  (let* ((xstream 
          (make-xstream 
           stream
           :name (make-stream-name
                  :entity-name "main document"
                  :entity-kind :main
                  :file-name (or (ignore-errors (pathname *standard-output*))
                                 *default-pathname-defaults*))
           :initial-speed 1))
         (zstream (make-zstream :input-stack (list xstream))))
    (apply #'p/document zstream handler args)))

(defun parse-dtd-file (filename)
  (with-open-file (s filename :element-type '(unsigned-byte 8))
    (parse-dtd-stream s)))

(defun parse-dtd-stream (stream)
  (let ((input (make-xstream stream)))
    (setf (xstream-name input)
          (make-stream-name
           :entity-name "dtd"
           :entity-kind :main
           :file-name (pathname stream)))
    (let ((zstream (make-zstream :input-stack (list input)))
          (*ctx* (make-context :handler nil))
          (*validate* t)
          (*data-behaviour* :DTD))
      (peek-rune input)
      (p/ext-subset zstream)
      (dtd *ctx*))))

(defun parse-string (string handler)
  ;; XXX this function mis-handles encoding
  (let* ((x (string->xstream string))
         (z (make-zstream :input-stack (list x))))
    (p/document z handler)))

(defun string->xstream (string)
  ;; XXX encoding is mis-handled by this kind of stream
  (make-rod-xstream (string-rod string)))

(defclass octet-input-stream (fundamental-binary-input-stream)
    ((octets :initarg :octets)
     (pos :initform 0)))

(defmethod stream-read-byte ((stream octet-input-stream))
  (with-slots (octets pos) stream
    (if (>= pos (length octets))
        :eof
        (prog1
            (elt octets pos)
          (incf pos)))))

(defmethod stream-read-sequence ((stream octet-input-stream) sequence
                                 &optional (start 0) (end (length sequence)))
  (with-slots (octets pos) stream
    (let* ((length (min (- end start) (- (length octets) pos)))
           (end1 (+ start length))
           (end2 (+ pos length)))
      (replace sequence octets :start1 start :end1 end1 :start2 pos :end2 end2)
      (setf pos end2)
      end1)))

(defun make-octet-input-stream (octets)
  (make-instance 'octet-input-stream :octets octets))

(defun parse-octets (octets handler &rest args)
  (apply #'parse-stream (make-octet-input-stream octets) handler args))

;;;;

#+allegro
(defmacro sp (&body body)
  `(progn
     (prof:with-profiling (:type :space) .,body)
     (prof:show-flat-profile)))

#+allegro
(defmacro tm (&body body)
  `(progn
     (prof:with-profiling (:type :time) .,body)
     (prof:show-flat-profile)))

;;;;

(defun zstream-push (new-xstream zstream)
  (cond ((find-if (lambda (x)
                    (and (xstream-p x)
                         (eql (stream-name-entity-name (xstream-name x))
                              (stream-name-entity-name (xstream-name new-xstream)))
                         (eql (stream-name-entity-kind (xstream-name x))
                              (stream-name-entity-kind (xstream-name new-xstream)))))
                  (zstream-input-stack zstream))
         (error "Infinite recursion.")))
  (push new-xstream (zstream-input-stack zstream))
  zstream)

(defun recurse-on-entity (zstream name kind continuation)
  (assert (not (zstream-token-category zstream)))
  ;;(sleep .2)
  ;;(warn "~S / ~S[~S]." (zstream-input-stack zstream) (mu name) kind)
  (call-with-entity-expansion-as-stream
   zstream
   (lambda (new-xstream)
     (push :stop (zstream-input-stack zstream))
     (zstream-push new-xstream zstream)
     (prog1
         (funcall continuation zstream)
       (assert (eq (peek-token zstream) :eof))
       (assert (eq (pop (zstream-input-stack zstream)) new-xstream))
       (close-xstream new-xstream)
       (assert (eq (pop (zstream-input-stack zstream)) :stop))
       (setf (zstream-token-category zstream) nil)
       '(consume-token zstream)) )
   name kind))

(defun merge-sysid (sysid base)
  (merge-pathnames sysid base))

(defun open-sysid (sysid)
  (open sysid  :element-type '(unsigned-byte 8) :direction :input))


;;;;

#|

(defparameter *test-files*
    '(;;"jclark:xmltest;not-wf;*;*.xml"
      "jclark:xmltest;valid;*;*.xml" 
      ;;"jclark:xmltest;invalid;*.xml"
      ))

(defun run-all-tests (&optional (test-files *test-files*))
  (let ((failed nil))
    (dolist (k test-files)
      (dolist (j (sort (directory k) #'string< :key #'pathname-name))
        (unless (test-file j)
          (push j failed))))
    (fresh-line)
    (cond (failed
           (write-string "**** Test failed on")
           (dolist (k failed)
             (format t "~%****  ~S." k))
           nil)
          (t
           (write-string "**** Test passed!")
           t))))

(defun test-file (filename)
  (let ((out-filename (merge-pathnames "out/" filename)))
    (if (probe-file out-filename)
        (positive-test-file filename out-filename)
      (negative-test-file filename))))

(defun positive-test-file (filename out-filename)
  (multiple-value-bind (nodes condition) 
      (ignore-errors (parse-file filename))
    (cond (condition
           (warn "**** Error in ~S: ~A." filename condition)
           nil)
          (t
           (let (res equal?)
             (setf res (with-output-to-string (sink)
                         (unparse-document nodes sink)))
             (setf equal?
               (with-open-file (in out-filename :direction :input :element-type 'character)
                 (do ((i 0 (+ i 1))
                      (c (read-char in nil nil) (read-char in nil nil)))
                     ((or (eq c nil) (= i (length res)))
                      (and (eq c nil) (= i (length res))))
                   (unless (eql c (char res i))
                     (return nil)))))
             (cond ((not equal?)
                    (format t "~&**** Test failed on ~S." filename)
                    (fresh-line)
                    (format t "** me: ~A" res)
                    (fresh-line)
                    (format t "** he: " res)
                    (finish-output)
                    (with-open-file (in out-filename :direction :input :element-type 'character)
                      (do ((c (read-char in nil nil) (read-char in nil nil)))
                          ((eq c nil))
                        (write-char c)))
                    nil)
                   (t
                    t)))))))

(defun negative-test-file (filename)
  (multiple-value-bind (nodes condition) 
      (ignore-errors (parse-file filename))
    (declare (ignore nodes))
    (cond (condition
           t)
          (t
           (warn "**** negative test failed on ~S." filename)))))

|#

;;;;

#+(or)                                  ;was ist das?
(progn

  (defmethod dom:create-processing-instruction ((document null) target data)
    (declare (ignorable document target data))
    nil)

  (defmethod dom:append-child ((node null) child)
    (declare (ignorable node child))
    nil)

  (defmethod dom:create-element ((document null) name)
    (declare (ignorable document name))
    nil)

  (defmethod dom:set-attribute ((document null) name value)
    (declare (ignorable document name value))
    nil)

  (defmethod dom:create-text-node ((document null) data)
    (declare (ignorable document data))
    nil)

  (defmethod dom:create-cdata-section ((document null) data)
    (declare (ignorable document data))
    nil)
  )


#||
(defmacro read-data-until* ((predicate input res res-start res-end) &body body)
  ;; fast variant -- for now disabled for no apparent reason
  ;; -> res, res-start, res-end
  `(let* ((rptr (xstream-read-ptr ,input))
          (p0   rptr)
          (fptr (xstream-fill-ptr ,input))
          (buf  (xstream-buffer ,input))
          ,res ,res-start ,res-end)
    (declare (type fixnum rptr fptr p0)
             (type (simple-array read-element (*)) buf))
    (loop
      (cond ((%= rptr fptr)
             ;; underflow -- hmm inject the scratch-pad with what we
             ;; read and continue, while using read-rune and collecting
             ;; d.h. besser wäre hier auch while-reading zu benutzen.
             (setf (xstream-read-ptr ,input) rptr)
             (multiple-value-setq (,res ,res-start ,res-end)
               (with-rune-collector/raw (collect)
                 (do ((i p0 (%+ i 1)))
                     ((%= i rptr))
                   (collect (%rune buf i)))
                 (let (c)
                   (loop
                     (cond ((%= rptr fptr)
                            (setf (xstream-read-ptr ,input) rptr)
                            (setf c (peek-rune input))
                            (cond ((eq c :eof)
                                   (return)))
                            (setf rptr (xstream-read-ptr ,input)
                                  fptr (xstream-fill-ptr ,input)
                                  buf  (xstream-buffer ,input)))
                           (t
                            (setf c (%rune buf rptr))))
                     (cond ((,predicate c)
                            ;; we stop
                            (setf (xstream-read-ptr ,input) rptr)
                            (return))
                           (t
                            ;; we continue
                            (collect c)
                            (setf rptr (%+ rptr 1))) )))))
             (return))
            ((,predicate (%rune buf rptr))
             ;; we stop
             (setf (xstream-read-ptr ,input) rptr)
             (setf ,res buf ,res-start p0 ,res-end rptr)
             (return) )
            (t
             ;; we continue
             (setf rptr (%+ rptr 1))) ))
    ,@body )) 
||#

;(defun read-data-until (predicate input continuation)
;  )

(defmacro read-data-until* ((predicate input res res-start res-end) &body body)
  "Read data from `input' until `predicate' applied to the read char 
   turns true. Then execute `body' with `res', `res-start', `res-end'
   bound to denote a subsequence (of RUNEs) containing the read portion.
   The rune upon which `predicate' turned true is neither consumed from 
   the stream, nor included in `res'.

   Keep the predicate short, this it may be included more than once into
   the macro's expansion."
  ;;
  (let ((input-var (gensym))
        (collect (gensym))
        (c (gensym)))
    `(LET ((,input-var ,input))
       (MULTIPLE-VALUE-BIND (,res ,res-start ,res-end) 
           (WITH-RUNE-COLLECTOR/RAW (,collect)
             (LOOP
               (LET ((,c (PEEK-RUNE ,input-var)))
                 (COND ((EQ ,c :EOF) 
                        ;; xxx error message
                        (RETURN))
                       ((FUNCALL ,predicate ,c)
                        (RETURN))
                       (t
                        (,collect ,c)
                        (CONSUME-RUNE ,input-var))))))
         (LOCALLY
           ,@body)))))
  
(defun read-name-token (input)
  (read-data-until* ((lambda (rune)
                       (declare (type rune rune))
                       (not (name-rune-p rune))) 
                     input
                     r rs re)
                    (intern-name r rs re)))

(defun read-cdata (input)
  (read-data-until* ((lambda (rune)
                       (declare (type rune rune))
                       (or (%rune= rune #/<) (%rune= rune #/&)))
                     input
                     source start end)
                    (locally
                     (declare (type (simple-array rune (*)) source)
                              (type ufixnum start)
                              (type ufixnum end)
                              (optimize (speed 3) (safety 0)))
                     (let ((res (make-array (%- end start) :element-type 'rune)))
                       (declare (type (simple-array rune (*)) res))
                       (let ((i (%- end start)))
                         (declare (type ufixnum i))
                         (loop
                           (setf i (- i 1))
                           (setf (%rune res i) (%rune source (the ufixnum (+ i start))))
                           (when (= i 0)
                             (return))))
                       res))))

(defun internal-entity-expansion (name)
  (let ((def (get-entity-definition name :general (dtd *ctx*))))
    (unless def
      (error "Entity '~A' is not defined." (rod-string name)))
    (unless (eq :INTERNAL (car def))
      (error "Entity '~A' is not an internal entity." name))
    (or (caddr def)
        (car
         (setf (cddr def)
           (cons (find-internal-entity-expansion name) nil))))))

(defun find-internal-entity-expansion (name)
  (let ((zinput (make-zstream)))
    (with-rune-collector-3 (collect)
      (labels ((muffle (input)
                 (let (c)
                   (loop
                     (setf c (read-rune input))
                     (cond ((eq c :eof)
                            (return))
                           ((rune= c #/&)
                            (setf c (peek-rune input))
                            (cond ((rune= c #/#)
                                   (let ((c (read-numeric-entity input)))
                                     (%put-unicode-char c collect)))
                                  (t
                                   (unless (name-start-rune-p (peek-rune input))
                                     (error "Expecting name after &."))
                                   (let ((name (read-name-token input)))
                                     (setf c (read-rune input))
                                     (assert (rune= c #/\;))
                                     (recurse-on-entity 
                                      zinput name :general
                                      (lambda (zinput)
                                        (muffle (car (zstream-input-stack zinput)))))))))
                           ((and (rune= c #/<))
                            ;; xxx fix error message
                            (cerror "Eat them in spite of this."
                                    "For no apparent reason #\/< is forbidden in attribute values. ~
                                     You lost -- next time choose SEXPR syntax.")
                            (collect c))
                           ((space-rune-p c)
                            (collect #/space))
                           ((not (data-rune-p c))
                            (error "illegal char: ~S." c))
                           (t
                            (collect c)))))))
        (declare (dynamic-extent #'muffle))
        (recurse-on-entity 
         zinput name :general
         (lambda (zinput)
           (muffle (car (zstream-input-stack zinput))))) ))))

(defun resolve-entity (name handler dtd)
  (let ((*validate* nil))
    (if (get-entity-definition name :general dtd)
        (let* ((*ctx* (make-context :handler handler :dtd dtd))
               (input (make-zstream))
               (*data-behaviour* :DOC))
          (recurse-on-entity
           input name :general
           (lambda (input)
             (prog1
                 (ecase (entity-source-kind name :general)
                   (:INTERNAL (p/content input))
                   (:EXTERNAL (p/ext-parsed-ent input)))
               (unless (eq (peek-token input) :eof)
                 (error "Trailing garbage. - ~S" (peek-token input)))))))
        nil)))

(defun read-att-value-2 (input)
  (let ((delim (read-rune input)))
    (unless (member delim '(#/\" #/\') :test #'eql)
      (error "Bad attribute value delimiter ~S, must be either #\\\" or #\\\'."
             (if (< delim char-code-limit) (code-char delim) delim)))
    (with-rune-collector-4 (collect)
      (loop
        (let ((c (read-rune input)))
          (cond ((eq c :eof)
                 (error "EOF"))
                ((rune= c delim)
                 (return))
                ((rune= #/& c)
                 (multiple-value-bind (kind sem) (read-entity-ref input)
                   (ecase kind
                     (:NUMERIC
                      (%put-unicode-char sem collect))
                     (:NAMED
                      (let* ((exp (internal-entity-expansion sem))
                             (n (length exp)))
                        (declare (type (simple-array rune (*)) exp))
                        (do ((i 0 (%+ i 1)))
                            ((%= i n))
                          (collect (%rune exp i))))))))
                ((space-rune-p c)
                 (collect #/u+0020))
                (t
                 (collect c))))))))

;;;;;;;;;;;;;;;;;

;;; Namespace stuff

(defvar *default-namespace-bindings*
  '((#"" . nil)
    (#"xmlns" . #"http://www.w3.org/2000/xmlns/")
    (#"xml" . #"http://www.w3.org/XML/1998/namespace")))
    
;; We already know that name is part of a valid XML name, so all we
;; have to check is that the first rune is a name-start-rune and that
;; there is not colon in it.
(defun nc-name-p (name)
  (and (name-start-rune-p (rune name 0))
       (notany #'(lambda (rune) (rune= #/: rune)) name)))

(defun split-qname (qname)
  (declare (type runes:simple-rod qname))
  (let ((pos (position  #/: qname)))
    (if pos
	(let ((prefix (subseq qname 0 pos))
	      (local-name (subseq qname (1+ pos))))
	  (if (nc-name-p local-name)
	      (values prefix local-name)
	      (error "~S is not a valid NcName." local-name)))
	(values () qname))))
		 
(defun decode-qname (qname)
  "decode-qname name => namespace-uri, prefix, local-name"
  (declare (type runes:simple-rod qname))
  (multiple-value-bind (prefix local-name) (split-qname qname)
    (let ((uri (find-namespace-binding prefix)))
      (if uri
	  (values uri prefix local-name)
	  (values nil nil nil)))))


(defun find-namespace-binding (prefix)
  (cdr (or (assoc (or prefix #"") (namespace-bindings *ctx*) :test #'rod=)
	   (error "Undeclared namespace prefix: ~A" (rod-string prefix)))))

;; FIXME: Should probably be refactored by adding :start and :end to rod=/rod-equal
(defun rod-starts-with (prefix rod)
  (and (<= (length prefix) (length rod))
       (dotimes (i (length prefix) t)
         (unless (rune= (rune prefix i) (rune rod i))
           (return nil)))))

(defun xmlns-attr-p (attr-name)
  (rod-starts-with #.(string-rod "xmlns") attr-name))

(defun attrname->prefix (attrname)
  (if (< 5 (length attrname))
      (subseq attrname 6)
      nil))

(defun find-namespace-declarations (element attr-alist)
  (let ((result
         (mapcar #'(lambda (attr)
                     (cons (attrname->prefix (car attr)) (cdr attr)))
                 (remove-if-not #'xmlns-attr-p attr-alist :key #'car))))
    ;; Argh!  PROCESS-ATTRIBUTES needs to know the attributes' namespaces
    ;; already.  But namespace declarations can be done using default values
    ;; in the DTD.  So we need to handle defaulting of attribute values twice,
    ;; once for xmlns attributes, then for all others.  (I really hope I'm
    ;; wrong on this one, but I don't see how.)
    (let ((e (find-element element (dtd *ctx*))))
      (when e
        (dolist (ad (elmdef-attributes e)) ;handle default values
          (let* ((name (attdef-name ad))
                 (prefix (attrname->prefix name)))
            (when (and (xmlns-attr-p name)
                       (not (member prefix result :key #'car :test #'rod=))
                       (listp (attdef-default ad)) ;:DEFAULT or :FIXED
                       )
              (push (cons prefix (cadr (attdef-default ad))) result))))))
    result))

(defun declare-namespaces (element attr-alist)
  (let ((ns-decls (find-namespace-declarations element attr-alist)))
    (dolist (ns-decl ns-decls )
      ;; check some namespace validity constraints
      ;; FIXME: Would be nice to add "this is insane, go ahead" restarts
      (let ((prefix (car ns-decl))
	    (uri (if (rod= #"" (cdr ns-decl))
		     nil
		     (cdr ns-decl))))
	(cond
	  ((and (rod= prefix #"xml")
		(not (rod= uri #"http://www.w3.org/XML/1998/namespace")))
	   (error "Attempt to rebind the prefix \"xml\" to ~S." (mu uri)))
	  ((and (rod= uri #"http://www.w3.org/XML/1998/namespace")
		(not (rod= prefix #"xml")))
	   (error "The namespace URI \"http://www.w3.org/XML/1998/namespace\" ~
                   may not be bound to the prefix ~S, only \"xml\" is legal."
		  (mu prefix)))
	  ((and (rod= prefix #"xmlns")
		(rod= uri #"http://www.w3.org/2000/xmlns/"))
	   (error "Attempt to bind the prefix \"xmlns\" to its predefined ~
                   URI \"http://www.w3.org/2000/xmlns/\", which is ~
                   forbidden for no good reason."))
	  ((rod= prefix #"xmlns")
	   (error "Attempt to bind the prefix \"xmlns\" to the URI ~S, ~
                   but it may not be declared." (mu uri)))
	  ((rod= uri #"http://www.w3.org/2000/xmlns/")
	   (error "The namespace URI \"http://www.w3.org/2000/xmlns/\" may ~
                   not be bound to prefix ~S (or any other)." (mu prefix)))
	  ((and (rod= uri #"") prefix)
	   (error "Only the default namespace (the one without a prefix) may ~
                   be bound to an empty namespace URI, thus undeclaring it."))
	  (t
	   (push (cons prefix uri) (namespace-bindings *ctx*))
	   (sax:start-prefix-mapping (handler *ctx*) (car ns-decl) (cdr ns-decl))))))
    ns-decls))

(defun undeclare-namespaces (ns-decls)
  (dolist (ns-decl ns-decls)
    (setf (namespace-bindings *ctx*) (delete ns-decl (namespace-bindings *ctx*)))
    (sax:end-prefix-mapping (handler *ctx*) (car ns-decl))))

(defun build-attribute-list-no-ns (attr-alist)
  (mapcar #'(lambda (pair) (make-attribute :qname (car pair) :value (cdr pair) :specified-p t))
	  attr-alist))

;; FIXME: Use a non-braindead way to enforce attribute uniqueness
(defun build-attribute-list-ns (attr-alist)
  (let (attributes)
    (dolist (pair attr-alist)
      (push (build-attribute (car pair) (cdr pair) t) attributes))
    
    ;; 5.3 Uniqueness of Attributes
    ;; In XML documents conforming to [the xmlns] specification, no
    ;; tag may contain two attributes which:
    ;; 1. have identical names, or
    ;; 2. have qualified names with the same local part and with
    ;; prefixes which have been bound to namespace names that are
    ;; identical.
    ;;
    ;; 1. is checked by read-tag-2, so we only deal with 2 here
    (do ((sublist attributes (cdr sublist)))
	((null sublist) attributes)
      (let ((attr-1 (car sublist)))
	(when (and (attribute-namespace-uri attr-1)
		   (find-if #'(lambda (attr-2)
				(and (rod= (attribute-namespace-uri attr-1)
					   (attribute-namespace-uri attr-2))
				     (rod= (attribute-local-name attr-1)
					   (attribute-local-name attr-2))))
		       (cdr sublist)))
	  (error "Multiple definitions of attribute ~S in namespace ~S."
		 (mu (attribute-local-name attr-1))
		 (mu (attribute-namespace-uri attr-1))))))))
    
(defun build-attribute (name value specified-p)
  (multiple-value-bind (prefix local-name) (split-qname name)
    (declare (ignorable local-name))
    (if (or (not prefix) ;; default namespace doesn't apply to attributes
	    (and (rod= #"xmlns" prefix) (not sax:*use-xmlns-namespace*)))
	(make-attribute :qname name
                        :value value
                        :specified-p specified-p)
	(multiple-value-bind (uri prefix local-name)
	    (decode-qname name)
	  (declare (ignore prefix))
	  (make-attribute :qname name
			  :value value
			  :namespace-uri uri
			  :local-name local-name
                          :specified-p specified-p)))))
    
;;; Faster constructors

;; Since using the general DOM interface to construct the parsed trees
;; may turn out to be quite expensive (That depends on the underlying
;; DOM implementation). A particular DOM implementation may choose to
;; implement an XML:FAST-CONSTRUCTORS method:

;; XML:FAST-CONSTRUCTORS document                               [method]
;;
;; Return an alist of constructors suitable for the document `document'.
;;
;;  (:MAKE-TEXT document parent data)
;;  (:MAKE-PROCESSING-INSTRUCTION document parent target content)
;;  (:MAKE-NODE document parent attributes content)
;;  [`attributes' now in turn is an alist]
;;  (:MAKE-CDATA document parent data)
;;  (:MAKE-COMMENT document parent data)
;;

;;;;;;;;;;;;;;;;;

;; System Identifier Protocol

;; A system identifier is an object obeying to the system identifier
;; protocol. Often something like an URL or a pathname.

;; OPEN-SYS-ID sys-id                                   [generic function]
;;
;; Opens the resource associated with the system identifier `sys-id'
;; for reading and returns a stream. For now it is expected, that the
;; stream is an octet stream (one of element type (unsigned-byte 8)).
;;
;; More precisely: The returned object only has to obey to the xstream
;; controller protocol. (That is it has to provide implementations for
;; READ-OCTETS and XSTREAM-CONTROLLER-CLOSE).

;; MERGE-SYS-ID sys-id base                             [generic function]
;;
;; Merges two system identifiers. That is resolve `sys-id' relative to
;; `base' yielding an absolute system identifier suitable for
;; OPEN-SYS-ID.

;; xstream Controller Protocol
;;
;; 


#||
(defun xml-parse (system-id &key document standalone-p)
  )
||#

;;;;;;;;;;;;;;;;;

;;; SAX validation handler

(defclass validator ()
    ((context :initarg :context :accessor context)
     (cdatap :initform nil :accessor cdatap)))

(defun make-validator (dtd root)
  (make-instance 'validator
    :context (make-context
              :handler nil
              :dtd dtd
              :model-stack (list (make-root-model root)))))

(macrolet ((with-context ((validator) &body body)
             `(let ((*ctx* (context ,validator))
                    (*validate* t))
                ,@body)))
  (defmethod sax:start-element ((handler validator) uri lname qname attributes)
    uri lname
    (with-context (handler)
      (validate-start-element *ctx* qname)
      (process-attributes *ctx* qname attributes)))

  (defmethod sax:start-cdata ((handler validator))
    (setf (cdatap handler) t))

  (defmethod sax:characters ((handler validator) data)
    (with-context (handler)
      (validate-characters *ctx* (if (cdatap handler) #"hack" data))))

  (defmethod sax:end-cdata ((handler validator))
    (setf (cdatap handler) nil))

  (defmethod sax:end-element ((handler validator) uri lname qname)
    uri lname
    (with-context (handler)
      (validate-end-element *ctx* qname))))
