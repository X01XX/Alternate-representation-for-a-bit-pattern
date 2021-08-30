;;;; Implement a Bits class, where a bits object is made up of a ones mask and a zeros mask.

;;;; Run with clisp bits.lisp

(load 'mask.lisp)

;;; Define the Bits class
(defclass bits ()
    ((ones-mask  :accessor bits-ones)
     (zeros-mask :accessor bits-zeros)))

;;; Initialize a new Bits instance
(defmethod initialize-instance
    :after
       ((bits-instance bits)
        &key ((:bits-ones  ones)  0 ones-supplied)
             ((:bits-zeros zeros) 0 zeros-supplied))

    (unless (and ones-supplied zeros-supplied)
        (error "bits initialize-instance: must provide :bit-ones <mask> :bits-zeros <mask>"))

    (unless (and (eq (type-of ones) 'MASK) (eq (type-of zeros) 'MASK))
           (error "bits initialize-instance: must provide :bit-ones <mask> :bits-zeros <mask>"))

    (setf (bits-ones  bits-instance) ones)
    (setf (bits-zeros bits-instance) zeros))

;;; Return true if a Bits instance has any X bit positions
(defmethod any-x ((bits-instance bits))
    (> (logand (mask-ones (bits-ones bits-instance)) (mask-ones (bits-zeros bits-instance))) 0))

;;; Return the position mask of a Bits object
(defmethod position-mask ((bits-instance bits))
    (mask-or (bits-ones bits-instance) (bits-zeros bits-instance)))

;;; Print a Bits object
(defmethod bits-print ((bits-instance bits))
    ;; Get a mask of the bit positions
    (let ((bit-positions (mask-ones (position-mask bits-instance)))

          ;; Split single bit positions from the position mask
          (position-masks nil)
          (anum1 0) (anum0 0))
          
    (loop while (> bit-positions 0) do
    
        (setf previous-positions bit-positions)
        (setf bit-positions (logand bit-positions (- bit-positions 1)))
        (setf position-masks (cons (logxor bit-positions previous-positions) position-masks)))

    ;; Print each bit position
    (loop while position-masks do
       
       ;; Get bit position values for zero and one
       (setf anum1 (logand (car position-masks) (mask-ones (bits-ones bits-instance))))
       (setf anum0 (logand (car position-masks) (mask-ones (bits-zeros bits-instance))))

       ;; Print a character based on the bit position values for zero and one
       (cond ((and (> anum1 0) (> anum0 0)) (princ "X"))
             ((> anum1 0) (princ "1"))
             ((> anum0 0) (princ "0"))
             (t (printc "?")))

       ;; Discard bit position mask that was just processed
       (setf position-masks (cdr position-masks)))))

;;; Return the bitwise NOT of a bits instance
(defmethod bits-not ((bits-instance bits))

    (if (any-x bits-instance) (error "bits-not X-bit detected in bits-instance"))

    (make-instance 'bits :bits-ones  (bits-zeros  bits-instance)
                         :bits-zeros (bits-ones   bits-instance)))

;;; Return true if two Bits intersect
(defmethod bits-intersectp ((bits-in1 bits) (bits-in2 bits))

    (let ((bit-positions1 (position-mask bits-in1))
          (bit-positions2 (position-mask bits-in2)))

        (unless (mask-equalp bit-positions1 bit-positions2) (error "bits-intersect: incompatible bits arguments"))

        (zerop (mask-ones (mask-and (mask-xor (bits-ones bits-in1) (bits-ones bits-in2)) (mask-xor (bits-zeros bits-in1) (bits-zeros bits-in2)))))))

;;; Return the intersection of two Bits
(defmethod bits-intersection ((bits-in1 bits) (bits-in2 bits))

    (let ((bit-positions1 (position-mask bits-in1))
          (bit-positions2 (position-mask bits-in2)))

        (unless (mask-equalp bit-positions1 bit-positions2) (error "bits-intersection: incompatible bits arguments"))

        (unless (bits-intersectp bits-in1 bits-in2) (error "bits-intersection: arguments don't intersect"))

        (make-instance 'bits :bits-ones  (mask-and (bits-ones  bits-in1) (bits-ones  bits-in2))
                             :bits-zeros (mask-and (bits-zeros bits-in1) (bits-zeros bits-in2)))))

;;; Return the union of two Bits
(defmethod bits-union ((bits-in1 bits) (bits-in2 bits))

    (let ((bit-positions1 (position-mask bits-in1))
          (bit-positions2 (position-mask bits-in2)))

        (unless (mask-equalp bit-positions1 bit-positions2) (error "bits-union: incompatible bits arguments"))

        (make-instance 'bits :bits-ones  (mask-or (bits-ones  bits-in1) (bits-ones  bits-in2))
                             :bits-zeros (mask-or (bits-zeros bits-in1) (bits-zeros bits-in2)))))

;;; Return the bitwise AND of two Bits
(defmethod bits-and ((bits-in1 bits) (bits-in2 bits))

    (let ((bit-positions1 (position-mask bits-in1))
          (bit-positions2 (position-mask bits-in2)) (ones 0))
    
        (unless (mask-equalp bit-positions1 bit-positions2) (error "bits-and: incompatible bits arguments"))

        (if  (any-x bits-in1) (error "bits-and: first argument contains and X bit position"))
        (if  (any-x bits-in2) (error "bits-and: second argument contains and X bit position"))

        (setf ones (mask-and (bits-ones  bits-in1) (bits-ones  bits-in2)))

        (make-instance 'bits :bits-ones  ones
                             :bits-zeros (mask-xor ones bit-positions1))))

;;; Return the bitwise OR of two Bits
(defmethod bits-or ((bits-in1 bits) (bits-in2 bits))

    (let ((bit-positions1 (position-mask bits-in1))
          (bit-positions2 (position-mask bits-in2)) (ones 0))
    
        (unless (mask-equalp bit-positions1 bit-positions2) (error "bits-or: incompatible bits arguments"))

        (if  (any-x bits-in1) (error "bits-or: first argument contains and X bit position"))
        (if  (any-x bits-in2) (error "bits-or: second argument contains and X bit position"))

        (setf ones (mask-or (bits-ones  bits-in1) (bits-ones  bits-in2)))

        (make-instance 'bits :bits-ones  ones
                             :bits-zeros (mask-xor ones bit-positions1))))

;;; Return the bitwise XOR of two Bits
(defmethod bits-xor ((bits-in1 bits) (bits-in2 bits))

    (let ((bit-positions1 (position-mask bits-in1))
          (bit-positions2 (position-mask bits-in2)) (ones 0))
    
        (unless (mask-equalp bit-positions1 bit-positions2) (error "bits-or: incompatible bits arguments"))

        (if  (any-x bits-in1) (error "bits-or: first argument contains and X bit position"))
        (if  (any-x bits-in2) (error "bits-or: second argument contains and X bit position"))

        (setf ones (mask-xor (bits-ones  bits-in1) (bits-ones  bits-in2)))

        (make-instance 'bits :bits-ones  ones
                             :bits-zeros (mask-xor ones bit-positions1))))

; Test code

(format t "~&bits_test.lisp")

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
(princ "not ")
(bits-print bits-instance3)
(setf x2 (bits-not bits-instance3))
(princ " = ")
(bits-print x2)

(format t "~&")
(bits-print bits-instance3)
(princ " and ")
(bits-print bits-instance4)
(setf x2 (bits-and bits-instance3 bits-instance4))
(princ " = ")
(bits-print x2)

(format t "~&")
(bits-print bits-instance3)
(princ " or  ")
(bits-print bits-instance4)
(setf x2 (bits-or bits-instance3 bits-instance4))
(princ " = ")
(bits-print x2)

(format t "~&")
(bits-print bits-instance3)
(princ " xor ")
(bits-print bits-instance4)
(setf x2 (bits-xor bits-instance3 bits-instance4))
(princ " = ")
(bits-print x2)

(format t "~&")
(bits-print bits-instance1)
(princ " intersection ")
(bits-print bits-instance2)
(setf x1 (bits-intersection bits-instance1 bits-instance2))
(princ " = ")
(bits-print x1)

(format t "~&")
(bits-print bits-instance3)
(princ " union ")
(bits-print bits-instance4)
(setf x2 (bits-union bits-instance3 bits-instance4))
(princ " = ")
(bits-print x2)

; Should look like
; not 0101 = 1010
; 0101 and 1100 = 0100
; 0101 or  1100 = 1101
; 0101 xor 1100 = 1001
; 011X intersection 0111 = 0111
; 0101 union 1100 = X10X

