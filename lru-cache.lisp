#|
 This file is a part of lru-cache
 (c) 2023 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:org.shirakumo.lru-cache
  (:use #:cl)
  (:export
   #:lru-cache
   #:make-lru-cache
   #:lru-cache-size
   #:lru-cache-push
   #:lru-cache-pop
   #:lru-cache-id
   #:lru-cache-evict
   #:lru-cache-clear
   #:lru-cache-count
   #:map-lru-cache
   #:do-lru-cache))

(in-package #:org.shirakumo.lru-cache)

(defstruct (lru-cache-node
            (:constructor make-lru-cache-node (left right id))
            (:predicate NIL)
            (:copier NIL))
  (left NIL :type T)
  (right NIL :type T)
  (value NIL :type T)
  (id 0 :type (unsigned-byte 32)))

(defmethod print-object ((node lru-cache-node) stream)
  (print-unreadable-object (node stream :type T :identity T)
    (format stream "~a" (lru-cache-node-value node))))

(defstruct (lru-cache
            (:constructor %make-lru-cache (head table size))
            (:predicate NIL)
            (:copier NIL))
  (head NIL :type lru-cache-node)
  (table NIL :type hash-table)
  (size 0 :type (unsigned-byte 32) :read-only T))

(defmethod print-object ((cache lru-cache) stream)
  (print-unreadable-object (cache stream :type T :identity T)
    (format stream "~a" (lru-cache-size cache))))

(defmethod describe-object ((cache lru-cache) stream)
  (format stream "~a~%  [~s]~%~%" cache (type-of cache))
  (loop with tail = (lru-cache-node-left (lru-cache-head cache))
        for i from 0
        for node = (lru-cache-head cache) then (lru-cache-node-right node)
        for value = (lru-cache-node-value node)
        until (eq node tail)
        do (when value
             (format stream "~3d ~a~%" (lru-cache-node-id node) value))))

(defun make-lru-cache (size)
  (check-type size (integer 1))
  (let ((head (make-lru-cache-node NIL NIL 0)))
    (setf (lru-cache-node-left head) head)
    (setf (lru-cache-node-right head) head)
    (loop for i from 1 below size
          for node = (make-lru-cache-node head (lru-cache-node-right head) i)
          do (setf (lru-cache-node-right (lru-cache-node-left head)) node)
             (setf (lru-cache-node-left head) node))
    (%make-lru-cache head (make-hash-table :test 'eq :size size) size)))

(defun lru-cache-push (value cache)
  (declare (optimize speed (safety 1)))
  (declare (type lru-cache cache))
  (let* ((table (lru-cache-table cache))
         (head (lru-cache-head cache))
         (node (gethash value table)))
    (cond ((null node)
           ;; Claim the oldest (left of the head) node and shift up
           (let ((prev (the lru-cache-node (lru-cache-node-left head))))
             (setf (lru-cache-node-value prev) value)
             (setf (lru-cache-head cache) prev)
             (when (lru-cache-node-value prev)
               (remhash (lru-cache-node-value prev) table))
             (setf (gethash value table) prev)
             (lru-cache-node-id prev)))
          ((eq node head)
           NIL)
          (T
           (let ((l (the lru-cache-node (lru-cache-node-left node)))
                 (r (the lru-cache-node (lru-cache-node-right node))))
             ;; Fuse neighbours to remove the node
             (setf (lru-cache-node-right l) r)
             (setf (lru-cache-node-left r) l)
             ;; Inject the node as the new head
             (setf (lru-cache-head cache) node)
             (setf (lru-cache-node-left node) (lru-cache-node-left head))
             (setf (lru-cache-node-right node) head)
             (setf (lru-cache-node-left head) node)
             NIL)))))

(defun lru-cache-pop (value cache)
  (declare (optimize speed (safety 1)))
  (declare (type lru-cache cache))
  (let* ((table (lru-cache-table cache))
         (head (lru-cache-head cache))
         (node (gethash value table)))
    (cond ((null node)
           NIL)
          ((eq node head)
           (setf (lru-cache-node-value node) NIL)
           (remhash value table)
           (setf (lru-cache-head cache) (lru-cache-node-right node))
           (lru-cache-node-id node))
          (T
           (let ((l (the lru-cache-node (lru-cache-node-left node)))
                 (r (the lru-cache-node (lru-cache-node-right node))))
             ;; Fuse neighbours to remove the node
             (setf (lru-cache-node-right l) r)
             (setf (lru-cache-node-left r) l)
             ;; Inject the node as the new tail
             (setf (lru-cache-node-right node) (lru-cache-node-left head))
             (setf (lru-cache-node-right node) head)
             (setf (lru-cache-node-left head) node)
             (lru-cache-node-id node))))))

(defun lru-cache-evict (cache)
  (declare (optimize speed (safety 1)))
  (declare (type lru-cache cache))
  (when (lru-cache-node-value (lru-cache-head cache))
    (loop with head = (lru-cache-head cache)
          for node = (lru-cache-node-left head) then (lru-cache-node-left node)
          for value = (lru-cache-node-value node)
          until (eq head node)
          do (when value
               (setf (lru-cache-node-value node) NIL)
               (remhash value (lru-cache-table cache))
               (return (values value (lru-cache-node-id node)))))))

(defun lru-cache-clear (cache)
  (declare (optimize speed (safety 1)))
  (declare (type lru-cache cache))
  (clrhash (lru-cache-table cache))
  (loop with tail = (lru-cache-node-left (lru-cache-head cache))
        for node = (lru-cache-head cache) then (lru-cache-node-right node)
        until (eq tail node)
        do (setf (lru-cache-node-value node) NIL))
  cache)

(defun lru-cache-id (value cache)
  (declare (optimize speed (safety 1)))
  (declare (type lru-cache cache))
  (let ((node (gethash value (lru-cache-table cache))))
    (when node
      (lru-cache-node-id (the lru-cache-node node)))))

(defun lru-cache-count (cache)
  (declare (optimize speed (safety 1)))
  (declare (type lru-cache cache))
  (loop with tail = (lru-cache-node-left (lru-cache-head cache))
        for node = (lru-cache-head cache) then (lru-cache-node-right node)
        until (or (eq tail node) (null (lru-cache-node-value node)))
        count T))

(defun map-lru-cache (function cache)
  (declare (optimize speed (safety 1)))
  (declare (type lru-cache cache))
  (loop with tail = (lru-cache-node-left (lru-cache-head cache))
        with function = (etypecase function
                          (symbol (fdefinition function))
                          (function function))
        for node = (lru-cache-head cache) then (lru-cache-node-right node)
        for value = (lru-cache-node-value node)
        until (eq tail node)
        do (if value
               (funcall function value (lru-cache-node-id node))
               (return)))
  cache)

(defmacro do-lru-cache ((element id cache &optional result) &body body)
  (let ((cacheg (gensym "CACHE"))
        (tail (gensym "TAIL"))
        (node (gensym "NODE")))
    `(loop with ,cacheg = ,cache
           with ,tail of-type lru-cache-node = (lru-cache-node-left (lru-cache-head ,cacheg))
           for ,node of-type lru-cache-node = (lru-cache-head ,cacheg) then (lru-cache-node-right ,node)
           until (eq ,tail ,node)
           do (let ((,element (lru-cache-node-value ,node)))
                (cond (,element
                       (let ((,id (lru-cache-node-id node)))
                         (declare (ignorable ,id))
                         ,@body))
                      (T
                       (return ,result)))))))
