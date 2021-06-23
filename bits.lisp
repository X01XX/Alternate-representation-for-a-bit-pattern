;;;; Implement a bits class, where a bits object is made up of a ones mask and a zeros mask.

(load 'mask.lisp)

;; Define the bits class
(defclass bits ()
    ((ones-mask  :accessor bits-ones)
     (zeros-mask :accessor bits-zeros)))

;; Initialize a new bits instance
(defmethod initialize-instance
    :after
       ((bits-instance bits)
        &key ((:bits-ones  ones)  0 ones-supplied)
             ((:bits-zeros zeros) 0 zeros-supplied))

    (unless (and ones-supplied zeros-supplied)
        (error "bits initialize-instance: must provide :bit-ones <mask> :bits-zeros <mask>"))

    (when (and ones-supplied zeros-supplied)

        (unless (and (eq (type-of ones) 'MASK) (eq (type-of zeros) 'MASK))
           (error "bits initialize-instance: must provide :bit-ones <mask> :bits-zeros <mask>"))

        (setf (bits-ones  bits-instance) ones)
        (setf (bits-zeros bits-instance) zeros)))

;; Return the position mask of a bits instance
(defmethod position-mask ((bits-instance bits))
    (make-instance 'mask :ones (logior (mask-ones (bits-ones bits-instance)) (mask-ones (bits-zeros bits-instance))))
)

;; Print a bits instance
(defmethod bits-print ((bits-instance bits))
    ; Get a mask of the bit positions
    (setf bit-positions (mask-ones (position-mask bits-instance)))

    ; Split single bit positions from the position mask
    (setf position-masks nil)
    (loop while (> bit-positions 0) do
    
        (setf previous-positions bit-positions)
        (setf bit-positions (logand bit-positions (- bit-positions 1)))
        (setf position-masks (cons (logxor bit-positions previous-positions) position-masks)))

    ; Print each bit position
    (loop while position-masks do
       
       ; Get bit position values for zero and one
       (setf anum1 (logand (car position-masks) (mask-ones (bits-ones bits-instance))))
       (setf anum0 (logand (car position-masks) (mask-ones (bits-zeros bits-instance))))

       ; Print a character based on the bit position values for zero and one
       (cond ((and (> anum1 0) (> anum0 0)) (princ "X"))
             ((> anum1 0) (princ "1"))
             ((> anum0 0) (princ "0"))
             (t (printc "?")))

       ; Discard bit position mask that was just processed
       (setf position-masks (cdr position-masks)))

    ;(format t "bits-instance1: ~v,'0b" num (mask-ones (bits-ones bits-instance1)))
)

; Test code
(setf bits-instance1 (make-instance 'bits :bits-ones  (make-instance 'mask :ones 13)
                                          :bits-zeros (make-instance 'mask :ones 17)))

; ones mask      01101
; zeros mask     10001
; positions mask 11101

(bits-print bits-instance1) ; should print 011X, skipping second bit that is 0 in positions mask

