(defpackage xmlconf
  (:use :cl)
  (:alias (:string-dom :dom)))
(in-package :xmlconf)

(defun relevant-test-p (test)
  (and (equal (dom:get-attribute test "TYPE") "valid")
       (let ((version (dom:get-attribute test "RECOMMENDATION")))
         (cond
           ((equal version "XML1.0")
             (cond
               ((equal (dom:get-attribute test "NAMESPACE") "no")
                 (warn "~A: test applies to parsers without namespace support, skipping"
                       (dom:get-attribute test "URI"))
                 nil)
               (t
                 t)))
           ((equal version "XML1.1")
             ;; not supported
             nil)
           (t
             (warn "unrecognized RECOMMENDATION value: ~A" version)
             nil)))))

(defun test-xml-conformance (directory)
  (let ((xmlconf (xml:parse-file (merge-pathnames "xmlconf.xml" directory))))
    (dolist (test (dom:get-elements-by-tag-name xmlconf "TEST"))
      (when (relevant-test-p test)
        (let* ((uri (dom:get-attribute test "URI"))
               (base
                (loop
                    for parent = test then (dom:parent-node parent)
                    for base = (dom:get-attribute parent "xml:base")
                    until base
                    finally (return base)))
               (pathname (merge-pathnames uri (merge-pathnames base directory))))
          (princ pathname)
          (unless (probe-file pathname)
            (error "file not found: ~A" pathname))
          (with-simple-restart (skip-test "Skip this test")
            (handler-case
                (progn
                  (mp:with-timeout (60)
                    (xml:parse-file pathname))
                  (format t " ok~%"))
              ((and serious-condition (not excl:interrupt-signal)) (c)
                (format t " FAILED:~%  ~A~%[~A]~%"
                        c
                        (dom:data (car (dom:child-nodes test))))))))))))
