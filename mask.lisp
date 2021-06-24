;;;; Implement a mask class 

;; Define the mask class
(defclass mask ()
  ((ones :accessor mask-ones)))

;; Initialize a new mask instance
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

;; Print a mask instance
(defmethod mask-print ((msk mask))
    (format t "~B" (mask-ones msk)))

(defmethod mask-and ((msk1 mask) (msk2 mask))
    (make-instance 'mask :ones (logand (mask-ones msk1) (mask-ones msk2))))

(defmethod mask-or ((msk1 mask) (msk2 mask))
    (make-instance 'mask :ones (logior (mask-ones msk1) (mask-ones msk2))))

(defmethod mask-xor ((msk1 mask) (msk2 mask))
    (make-instance 'mask :ones (logxor (mask-ones msk1) (mask-ones msk2))))

(defmethod mask-not-equal ((msk1 mask) (msk2 mask))
    (/= (mask-ones msk1) (mask-ones msk2)))
 
; Test code
; (setf msk1 (make-instance 'mask :ones 6))
; (mask-print msk1)

