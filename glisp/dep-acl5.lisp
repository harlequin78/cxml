;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: CL-USER; Encoding: utf-8; -*-
;;; ---------------------------------------------------------------------------
;;;     Title: ACL-5.0 dependent stuff + fixups
;;;   Created: 1999-05-25 22:32
;;;    Author: Gilbert Baumann <unk6@rz.uni-karlsruhe.de>
;;;   License: GPL (See file COPYING for details).
;;; ---------------------------------------------------------------------------
;;;  (c) copyright 1998,1999 by Gilbert Baumann

;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

;;; Changes
;;; =======

;;; When        Who     What
;;; ---------------------------------------------------------------------------
;;; 2002-01-04  GB      spend BLOCK for DEFSUBST
;;; 1999-08-31  SES     Stig Erik Sandø <stig@ii.uib.no>
;;;
;;;                     Changed #+allegro-v5.0 to 
;;;                     #+(and allegro-version>= (version>= 5))
;;;

(export 'glisp::read-byte-sequence :glisp)
(export 'glisp::read-char-sequence :glisp)
(export 'glisp::run-unix-shell-command :glisp)
(export 'glisp::mp/process-run-function :glisp)
(export 'glisp::mp/process-kill :glisp)
(export 'glisp::mp/current-process :glisp)
(export 'glisp::mp/seize-lock :glisp)
(export 'glisp::mp/release-lock :glisp)
(export 'glisp::mp/process-yield :glisp)
(export 'glisp::mp/process-wait :glisp)
(export 'glisp::getenv :glisp)

(export 'glisp::make-server-socket :glisp)

;; ACL is incapable to define compiler macros on (setf foo)
;; Unfortunately it is also incapable to declaim such functions inline.
;; So we revoke the DEFUN hack from dep-gcl here.

(defmacro glisp::defsubst (fun args &body body)
  (if (and (consp fun) (eq (car fun) 'setf))
      (let ((fnam (intern (concatenate 'string "(SETF " (symbol-name (cadr fun)) ")")
                          (symbol-package (cadr fun)))))
        `(progn
           (defsetf ,(cadr fun) (&rest ap) (new-value) (list* ',fnam new-value ap))
           (glisp::defsubst ,fnam ,args .,body)))
    (labels ((declp (x)
               (and (consp x) (eq (car x) 'declare))))
      `(progn
         (defun ,fun ,args .,body)
         (define-compiler-macro ,fun (&rest .args.)
           (cons '(lambda ,args
                   ,@(remove-if-not #'declp body)
                   (block ,fun 
                     ,@(remove-if #'declp body)))
                 .args.))))))
