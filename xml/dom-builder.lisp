;;; XXX this DOM builder knows too much about the specifics of the DOM
;;; implementation for my taste.  While document creation is not specified
;;; by the DOM Level 1 spec, we shouldn't really be manually setting slots
;;; in other nodes IMHO.
;;;
;;; As a follow-up to that, the children list is created in the wrong order
;;; and then reversed.  Is it really worth the improved speed to do this?
;;; Calling APPEND-NODE would be portable.
;;;
;;; In particular, that design choice has lead to other bugs, for example the
;;; PARENT slot has to be set manually, too.  A DOM test finally showed
;;; that this had been forgotten for Text nodes and PIs.
;;;
;;; Opinions?
;;;
;;;   -- David

(in-package :dom-impl)

(export 'dom-builder)

(defclass dom-builder ()
  ((document      :initform nil :accessor document)
   (element-stack :initform '() :accessor element-stack)))

(defmethod sax:start-document ((handler dom-builder))
  (let ((document (make-instance 'dom-impl::document)))
    (setf (slot-value document 'dom-impl::owner) document
	  (slot-value document 'dom-impl::doc-type) nil)
    (setf (document handler) document)
    (push document (element-stack handler))))

(defmethod sax:end-document ((handler dom-builder))
  (setf (slot-value (document handler) 'children )
	(nreverse (slot-value (document handler) 'children)))
  (setf (slot-value (document handler) 'entities) xml::*entities*)
  (document handler))

(defmethod sax:start-dtd ((handler dom-builder) name publicid systemid)
  (declare (ignore publicid systemid))
  (let ((document (document handler))
	(doctype (make-instance 'dom-impl::document-type
                   :name name
                   :notations (make-instance 'dom-impl::named-node-map)
                   :entities (make-instance 'dom-impl::named-node-map))))
    (setf (slot-value doctype 'dom-impl::owner) document
	  (slot-value document 'dom-impl::doc-type) doctype)))

(defmethod sax:start-element ((handler dom-builder) namespace-uri local-name qname attributes)
  (with-slots (document element-stack) handler
    (let ((element (cdom:create-element document qname))
	  (parent (car element-stack)))
      (dolist (attr attributes)
	(cdom:set-attribute element (xml::attribute-qname attr) (xml::attribute-value attr)))
      (setf (slot-value element 'dom-impl::parent) parent)
      (push element (slot-value parent 'dom-impl::children))
      (push element element-stack))))

(defmethod sax:end-element ((handler dom-builder) namespace-uri local-name qname)
  (let ((element (pop (element-stack  handler))))
    (setf (slot-value element 'dom-impl::children)
	  (nreverse (slot-value element 'dom-impl::children)))))

(defmethod sax:characters ((handler dom-builder) data)
  (with-slots (document element-stack) handler
    (let ((node (cdom:create-text-node document data))
          (parent (car element-stack)))
      (setf (slot-value node 'dom-impl::parent) parent)
      (push node (slot-value (car element-stack) 'dom-impl::children)))))

(defmethod sax:processing-instruction ((handler dom-builder) target data)
  (with-slots (document element-stack) handler
    (let ((node (cdom:create-processing-instruction document target data))
          (parent (car element-stack)))
      (setf (slot-value node 'dom-impl::parent) parent)
      (push node (slot-value (car element-stack) 'dom-impl::children)))))

(defmethod sax:comment ((handler dom-builder) data)
  (with-slots (document element-stack) handler
    (let ((node (cdom:create-comment document data))
          (parent (car element-stack)))
      (setf (slot-value node 'dom-impl::parent) parent)
      (push node (slot-value (car element-stack) 'dom-impl::children)))))

(defmethod sax:unparsed-entity-declaration
    ((handler dom-builder) name public-id system-id notation-name)
  (dom:set-named-item (dom:entities (dom:doctype (document handler)))
                      (make-instance 'dom-impl::entity
                        :name name
                        :public-id public-id
                        :system-id system-id
                        :notation-name notation-name)))
