(defpackage #:org.shirakumo.lru-cache
  (:use #:cl)
  (:export
   #:lru-cache
   #:make-lru-cache
   #:lru-cache-resize
   #:lru-cache-size
   #:lru-cache-push
   #:lru-cache-pop
   #:lru-cache-id
   #:lru-cache-evict
   #:lru-cache-clear
   #:lru-cache-count
   #:lru-cache-first
   #:map-lru-cache
   #:do-lru-cache
   #:lru-cache-list))

(in-package #:org.shirakumo.lru-cache)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct (lru-cache-node
              (:constructor make-lru-cache-node (left right id))
              (:predicate NIL)
              (:copier NIL))
    (left NIL :type T)
    (right NIL :type T)
    (value NIL :type T)
    (id 0 :type (unsigned-byte 32))))

(defmethod print-object ((node lru-cache-node) stream)
  (print-unreadable-object (node stream :type T :identity T)
    (format stream "~a [L ~d] ~d [R ~d]" 
            (lru-cache-node-value node)
            (lru-cache-node-id (lru-cache-node-left node))
            (lru-cache-node-id node)
            (lru-cache-node-id (lru-cache-node-right node)))))

(defstruct (lru-cache
            (:constructor %make-lru-cache (head table size))
            (:predicate NIL)
            (:copier NIL))
  (head NIL :type lru-cache-node)
  (table NIL :type hash-table)
  (size 0 :type (unsigned-byte 32)))

(defmethod print-object ((cache lru-cache) stream)
  (print-unreadable-object (cache stream :type T :identity T)
    (format stream "~d/~d" (lru-cache-count cache) (lru-cache-size cache))))

(defmethod describe-object ((cache lru-cache) stream)
  (format stream "~a~%  [~s]~%~%" cache (type-of cache))
  (loop with tail = (lru-cache-node-left (lru-cache-head cache))
        for i from 0
        for node = (lru-cache-head cache) then (lru-cache-node-right node)
        do (format stream "~a~%" node)
        until (eq node tail)))

(defun make-lru-cache (size &optional (test 'eql))
  (check-type size (integer 1))
  (let ((head (make-lru-cache-node NIL NIL (1- size))))
    (setf (lru-cache-node-left head) head)
    (setf (lru-cache-node-right head) head)
    (loop for i downfrom (- size 2) to 0
          for node = (make-lru-cache-node (lru-cache-node-left head) head i)
          do (setf (lru-cache-node-right (lru-cache-node-left head)) node)
             (setf (lru-cache-node-left head) node))
    (%make-lru-cache head (make-hash-table :test test :size size) size)))

(defun lru-cache-resize (cache size)
  (declare (type lru-cache cache))
  (check-type size (integer 1))
  (let ((head (lru-cache-head cache)))
    (cond ((< (lru-cache-size cache) size)
           (loop for i from (lru-cache-size cache) below size
                 for node of-type lru-cache-node = (make-lru-cache-node (lru-cache-node-left head) head i)
                 do (setf (lru-cache-node-right (lru-cache-node-left head)) node)
                    (setf (lru-cache-node-left head) node)))
          ((< size (lru-cache-size cache))
           ;; Find the element with the largest ID and evict it. We have to do this
           ;; in order to preserve ID consistency.
           (loop with table = (lru-cache-table cache)
                 for id downfrom (1- (lru-cache-size cache)) above size
                 for node = (loop for node of-type lru-cache-node = (lru-cache-node-left head) then (lru-cache-node-left node)
                                  do (when (= id (lru-cache-node-id node))
                                       (return node)))
                 do (setf (lru-cache-node-right (lru-cache-node-left node)) (lru-cache-node-right node))
                    (setf (lru-cache-node-left (lru-cache-node-right node)) (lru-cache-node-left node))
                    (when (lru-cache-node-value node)
                      (remhash (lru-cache-node-value node) table))
                    (when (eq head node)
                      (setf head (lru-cache-node-right node))
                      (setf (lru-cache-head cache) head)))))
    (setf (lru-cache-size cache) size)
    cache))

(defun check-lru-cache (cache &optional op)
  (let ((head (lru-cache-head cache))
        (visited (make-hash-table :test 'eq)))
    (loop for left = head then right
          for right = (lru-cache-node-right left)
          do (assert (eq left (lru-cache-node-left right)) ()
                     "~@[After ~a: ~]Bad back link in node:~%~a -> ~a~%~a <- ~a"
                     op left right (lru-cache-node-left right) right)
             (assert (null (gethash right visited)) ()
                     "~@[After ~a: ~]Bad cache cycle detected:~%~a"
                     op visited)
             (setf (gethash right visited) T)
          until (eq head right))
    (assert (= (hash-table-count visited) (lru-cache-size cache)) ()
            "~@[After ~a: ~]Node chain is smaller than expected:~%have ~d, should be ~d"
            op (hash-table-count visited) (lru-cache-size cache))))

(defun lru-cache-push (value cache)
  (declare (optimize speed (safety 1)))
  (declare (type lru-cache cache))
  (let* ((table (lru-cache-table cache))
         (head (lru-cache-head cache))
         (node (gethash value table)))
    (cond ((null node)
           ;; Claim the oldest (left of the head) node and shift up
           (let ((prev (the lru-cache-node (lru-cache-node-left head))))
             (when (lru-cache-node-value prev)
               (remhash (lru-cache-node-value prev) table))
             (setf (lru-cache-node-value prev) value)
             (setf (lru-cache-head cache) prev)
             (setf (gethash value table) prev)
             #++(check-lru-cache cache :push-a)
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
             (let ((tail (the lru-cache-node (lru-cache-node-left head))))
               (setf (lru-cache-node-left node) tail)
               (setf (lru-cache-node-right node) head)
               (setf (lru-cache-node-right tail) node)
               (setf (lru-cache-node-left head) node))
             (setf (lru-cache-head cache) node)
             #++(check-lru-cache cache :push-b)
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
           #++(check-lru-cache cache :pop-a)
           (lru-cache-node-id node))
          (T
           (setf (lru-cache-node-value node) NIL)
           (remhash value table)
           (let ((l (the lru-cache-node (lru-cache-node-left node)))
                 (r (the lru-cache-node (lru-cache-node-right node))))
             ;; Fuse neighbours to remove the node
             (setf (lru-cache-node-right l) r)
             (setf (lru-cache-node-left r) l)
             ;; Inject the node as the new tail
             (let ((tail (the lru-cache-node (lru-cache-node-left head))))
               (setf (lru-cache-node-right tail) node)
               (setf (lru-cache-node-left node) tail)
               (setf (lru-cache-node-right node) head)
               (setf (lru-cache-node-left head) node))
             #++(check-lru-cache cache :pop-b)
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
        for i from 0
        do (setf (lru-cache-node-value node) NIL)
           (setf (lru-cache-node-id node) i)
        until (eq tail node))
  cache)

(defun lru-cache-id (value cache)
  (declare (optimize speed (safety 1)))
  (declare (type lru-cache cache))
  (let ((node (gethash value (lru-cache-table cache))))
    (when node
      (lru-cache-node-id (the lru-cache-node node)))))

(defun lru-cache-first (cache)
  (declare (optimize speed (safety 1)))
  (declare (type lru-cache cache))
  (let ((head (lru-cache-head cache)))
    (values (lru-cache-node-value head)
            (lru-cache-node-id head))))

(defun lru-cache-count (cache)
  (declare (optimize speed (safety 1)))
  (declare (type lru-cache cache))
  (loop with tail = (lru-cache-node-left (lru-cache-head cache))
        for node = (lru-cache-head cache) then (lru-cache-node-right node)
        until (null (lru-cache-node-value node))
        count T
        until (eq tail node)))

(defun map-lru-cache (function cache)
  (declare (optimize speed (safety 1)))
  (declare (type lru-cache cache))
  (loop with tail = (lru-cache-node-left (lru-cache-head cache))
        with function = (etypecase function
                          (symbol (fdefinition function))
                          (function function))
        for node = (lru-cache-head cache) then (lru-cache-node-right node)
        for value = (lru-cache-node-value node)
        do (if value
               (funcall function value (lru-cache-node-id node))
               (return))
        until (eq tail node))
  cache)

(defmacro do-lru-cache ((element id cache &optional result) &body body)
  (let ((cacheg (gensym "CACHE"))
        (tail (gensym "TAIL"))
        (node (gensym "NODE")))
    `(loop with ,cacheg = ,cache
           with ,tail of-type lru-cache-node = (lru-cache-node-left (lru-cache-head ,cacheg))
           for ,node of-type lru-cache-node = (lru-cache-head ,cacheg) then (lru-cache-node-right ,node)
           do (let ((,element (lru-cache-node-value ,node)))
                (cond (,element
                       (let ((,id (lru-cache-node-id ,node)))
                         (declare (ignorable ,id))
                         ,@body))
                      (T
                       (loop-finish))))
           until (eq ,tail ,node)
           finally (return ,result))))

(defun lru-cache-list (cache)
  (let ((els ()))
    (do-lru-cache (element _ cache (nreverse els))
      (push element els))))
