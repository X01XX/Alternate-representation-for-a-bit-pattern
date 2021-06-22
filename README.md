# Alternate-representation-for-a-bit-pattern
An alternate way of representing a bit pattern, with some interesting consequences.

A bit pattern, like 0101, represents a number, 5.  It is assumed that zeros on the left go off to infinity, or at least to the end of the register, word, integer, etc.

The infinity of zeros is easy to ignore in most cases, however a NOT operation turns them into an infinity of ones.  The infinity of ones may be useful in some cases, not in others.

Another way to represent 5 is by using two masks, a Ones-mask: 0101, and a Zeros-mask: 1010.

By doing a bitwize OR operation on the two masks, we get a "position mask", 1111.

The position mask indicates that we are dealing with four contiguous bits.  The position mask bits need not be contiguous, but operations on bit patterns of this sort require the bit patterns to have the same position mask, with the result having the same position mask.

Some might be put off by the apparent inefficiency of representing a bit pattern with two masks.  If you allow for a 1 bit in the same position of both masks, you can represent a power of two bit patterns.  This use can be thought of as a region on a Karnaugh Map (having 1 or 2 bits per axis, no more).

A NOT operation on the Ones-mask is ~0101 & 1111 = 1010.  A NOT operation on the Zeros-mask is ~1010 & 1111 = 0101.  The result is a representation of the bit pattern 1010, with position mask 1111.
