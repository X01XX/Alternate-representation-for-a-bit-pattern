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

;; Return true if a bits instance has any X bit positions
(defmethod any-x ((bits-instance bits))
;    (format t " bits-ones:  ~4,'0b" (mask-ones (bits-ones bits-instance)))
;    (format t " bits-zeros: ~4,'0b " (mask-ones (bits-zeros bits-instance)))
;    (format t " logand: ~4,'0b " (logand (mask-ones (bits-ones bits-instance)) (mask-ones (bits-zeros bits-instance))))
    (> (logand (mask-ones (bits-ones bits-instance)) (mask-ones (bits-zeros bits-instance))) 0)
)

;; Return the position mask of a bits instance
(defmethod position-mask ((bits-instance bits))
    (mask-or (bits-ones bits-instance) (bits-zeros bits-instance))
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

(defmethod bits-not ((bits-instance bits))

    (if (any-x bits-instance) (error "bits-not X-bit detected in bits-instance"))

    (setf bit-positions (position-mask bits-instance))

    (make-instance 'bits :bits-ones  (mask-xor (bits-ones  bits-instance) bit-positions)
                         :bits-zeros (mask-xor (bits-zeros bits-instance) bit-positions)))

(defmethod bits-intersect ((bits-in1 bits) (bits-in2 bits))

    (setf bit-positions1 (position-mask bits-in1))
    (setf bit-positions2 (position-mask bits-in2))
    
    (if (mask-not-equal bit-positions1 bit-positions2) (error "bits-intersect: incompatible bits argumants"))
    
    (zerop (mask-ones (mask-and (mask-xor (bits-ones bits-in1) (bits-ones bits-in2)) (mask-xor (bits-zeros bits-in1) (bits-zeros bits-in2)))))
)

(defmethod bits-and ((bits-in1 bits) (bits-in2 bits))

    (setf bit-positions1 (position-mask bits-in1))
    (setf bit-positions2 (position-mask bits-in2))
    
    (if (mask-not-equal bit-positions1 bit-positions2) (error "bits-and: incompatible bits argumants"))

    ;(format t "~&in1: ")
    ;(bits-print bits-in1)
    ;(format t " in2: ")
    ;(bits-print bits-in2)
    
    (cond ((bits-intersect bits-in1 bits-in2)

              (make-instance 'bits :bits-ones  (mask-and (bits-ones  bits-in1) (bits-ones  bits-in2))
                                   :bits-zeros (mask-and (bits-zeros bits-in1) (bits-zeros bits-in2))))
          (t 
              (make-instance 'bits :bits-ones  (mask-and (bits-ones  bits-in1) (bits-ones  bits-in2))
                                   :bits-zeros (mask-or  (bits-zeros bits-in1) (bits-zeros bits-in2))))))
                                   
; Test code
(setf bits-instance1 (make-instance 'bits :bits-ones  (make-instance 'mask :ones 7)
                                          :bits-zeros (make-instance 'mask :ones 9)))
; ones mask      0111
; zeros mask     1001
; positions mask 1111

(setf bits-instance2 (make-instance 'bits :bits-ones  (make-instance 'mask :ones 7)
                                          :bits-zeros (make-instance 'mask :ones 8)))

(setf bits-instance3 (make-instance 'bits :bits-ones  (make-instance 'mask :ones 5)
                                          :bits-zeros (make-instance 'mask :ones 10)))

(setf bits-instance4 (make-instance 'bits :bits-ones  (make-instance 'mask :ones 12)
                                          :bits-zeros (make-instance 'mask :ones 3)))
                                          

(format t "~&")
(bits-print bits-instance1)
(princ " and ")
(bits-print bits-instance2)
(setf x1 (bits-and bits-instance1 bits-instance2))
(princ " = ")
(bits-print x1)

(format t "~&")
(bits-print bits-instance3)
(princ " and ")
(bits-print bits-instance4)
(setf x2 (bits-and bits-instance3 bits-instance4))
(princ " = ")
(bits-print x2)

(setf bits-instance5 (make-instance 'bits :bits-ones  (make-instance 'mask :ones 14)
                                          :bits-zeros (make-instance 'mask :ones 9)))
                                          
(format t "~&")
(bits-print bits-instance1)
(princ " and ")
(bits-print bits-instance5)
(setf x3 (bits-and bits-instance1 bits-instance5))
(princ " = ")
(bits-print x3)

(setf bits-instance6 (make-instance 'bits :bits-ones  (make-instance 'mask :ones 15)
                                          :bits-zeros (make-instance 'mask :ones 9)))
                                          
(format t "~&")
(bits-print bits-instance1)
(princ " and ")
(bits-print bits-instance6)
(setf x4 (bits-and bits-instance1 bits-instance6))
(princ " = ")
(bits-print x4)
