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
                   :notations (make-hash-table :test #'equalp))))
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
    (let ((node (cdom:create-text-node document data)))
      (push node (slot-value (car element-stack) 'dom-impl::children)))))

(defmethod sax:processing-instruction ((handler dom-builder) target data)
  (with-slots (document element-stack) handler
    (let ((node (cdom:create-processing-instruction document target data)))
      (push node (slot-value (car element-stack) 'dom-impl::children)))))

(defmethod sax:comment ((handler dom-builder) data)
  (with-slots (document element-stack) handler
    (let ((node (cdom:create-comment document data)))
      (push node (slot-value (car element-stack) 'dom-impl::children)))))
