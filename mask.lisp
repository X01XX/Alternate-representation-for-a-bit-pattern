;;;; Implement a Mask class 

;;; Define the Mask class
(defclass mask ()
  ((ones :accessor mask-ones)))

;;; Initialize a new mask instance
(defmethod initialize-instance
    :after
       ((msk mask)
        &key ((:ones ones) 0 ones-supplied))

    (unless ones-supplied
        (error "mask initialize-instance: must provide :ones <number>"))

    (when ones-supplied

        (unless (numberp ones)
            (error "mask initialize-instance: must provide :ones <number>"))

        (setf (mask-ones msk) ones)))

;;; Print a Mask object
(defmethod mask-print ((msk mask))
    (format t "~B" (mask-ones msk)))

;;; Return the bitwise AND of two Masks
(defmethod mask-and ((msk1 mask) (msk2 mask))
    (make-instance 'mask :ones (logand (mask-ones msk1) (mask-ones msk2))))

;;; Return the bitwise OR of two masks
(defmethod mask-or ((msk1 mask) (msk2 mask))
    (make-instance 'mask :ones (logior (mask-ones msk1) (mask-ones msk2))))

;;; Return the bitwise XOR of two masks
(defmethod mask-xor ((msk1 mask) (msk2 mask))
    (make-instance 'mask :ones (logxor (mask-ones msk1) (mask-ones msk2))))

;;; Return True if two masks are equal
(defmethod mask-equalp ((msk1 mask) (msk2 mask))
    (= (mask-ones msk1) (mask-ones msk2)))

