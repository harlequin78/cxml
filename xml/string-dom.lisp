(defpackage :string-dom
  (:use))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (do-external-symbols (var :cdom)
    (let* ((home-package
            (if (member var '(cdom:data cdom:name cdom:value cdom:tag-name
                              cdom:node-name cdom:node-value
                              cdom:substring-data cdom:get-attribute
                              cdom:set-attribute cdom:public-id cdom:system-id
                              cdom:notation-name cdom:target))
                :string-dom
                :cdom))
           (symbol (intern (symbol-name var) home-package)))
      (import symbol :string-dom)
      (export (list symbol) :string-dom))))

(defpackage :string-dom-impl (:use :cl))
(in-package :string-dom-impl)

(defun rod-to-string (frob)
  (if (null frob)
      nil
      (map 'string #'code-char frob)))

(defun string-dom:data (node)		(rod-to-string (cdom:data node)))
(defun string-dom:name (node)		(rod-to-string (cdom:name node)))
(defun string-dom:value (node)		(rod-to-string (cdom:value node)))
(defun string-dom:tag-name (node)	(rod-to-string (cdom:tag-name node)))
(defun string-dom:node-name (node)	(rod-to-string (cdom:node-name node)))
(defun string-dom:node-value (node)	(rod-to-string (cdom:node-value node)))

(defun (setf string-dom:data) (newval node)
  (setf (cdom:data node) newval))

(defun (setf string-dom:value) (newval node)
  (setf (cdom:value node) newval))

(defun (setf string-dom:node-value) (newval node)
  (setf (cdom:node-value node) newval))

(defun string-dom:substring-data (node offset count)
  (rod-to-string (cdom:substring-data node offset count)))

(defun string-dom:get-attribute (elt name)
  (rod-to-string (cdom:get-attribute elt name)))

(defun string-dom:set-attribute (elt name value)
  (cdom:set-attribute elt (glisp:rod name) (glisp:rod value)))

(defun string-dom:public-id (node)
  (rod-to-string (cdom:public-id node)))

(defun string-dom:system-id (node)
  (rod-to-string (cdom:system-id node)))

(defun string-dom:notation-name (node)
  (rod-to-string (cdom:notation-name node)))

(defun string-dom:target (node)
  (rod-to-string (cdom:target node)))
