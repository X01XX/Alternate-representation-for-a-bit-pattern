# Alternate-representation-for-a-bit-pattern
An alternate way of representing a bit pattern, with some interesting consequences.

A bit pattern, like 0101, represents a number, 5.  It is assumed that zeros on the left go off to infinity, or at least to the end of the register, word, integer, etc.

The infinity of zeros is easy to ignore in most cases, however a NOT operation turns them into an infinity of ones.  The infinity of ones may be useful in some cases, not in others.

Another way to represent the number 5 is by using two masks, a Ones-mask: 0101, and a Zeros-mask: 1010.

By doing a bitwize OR operation on the two masks, we get a "position mask", 1111.

The position mask indicates that we are dealing with four contiguous bits.  The position mask bits need not be contiguous, there could be a zero in the same position of both masks. Operations on bit patterns of this sort require the bit patterns have the same position mask, with the result having the same position mask.

Some might be put off by the apparent inefficiency of representing a bit pattern with two masks.  If you allow for a 1 bit in the same position of both masks, you can represent a power of two bit patterns.  This use can be thought of as a region on a Karnaugh Map (having 1 or 2 bits per axis, no more).  So if we have Ones-mask: 1010 and Zeros-mask: 0111, that can be thought of as the bit pattern 10X0, representing the bit patterns 1000 and 1010.  With two X-bit positions, four bit patterns will be represented.

To do a NOT operation on the bit pattern 0101, the result Ones-mask is, ~0101 & 1111 = 1010, the result Zeros-mask is ~1010 & 1111 = 0101.
The result is a representation of the bit pattern 1010, with position mask 1111.

To do an AND operation on 0101 (1s: 0101, 0s: 1010) and 1100 (1s: 1100, 0s: 0011), Ones-mask is 0101 & 1100 = 0100.  Zeros-mask is 1010 + 0011 = 1011.
The result is a representation of the bit pattern 0100, with position mask 1111.

To do an OR operation on 0101 (1s: 0101, 0s: 1010) and 1100 (1s: 1100, 0s: 0011), Ones-mask is 0101 + 1100 = 1101.  Zeros-mask is 1010 & 0011 = 0010.
The result is a representation of the bit pattern 1101, with position mask 1111.

To do an XOR operation on 0101 (1s: 0101, 0s: 1010) and 1100 (1s: 1100, 0s: 0011), Ones-mask is (0101 ^ 1010) & 1111 = 1001.  Zeros-mask is ~1001 & 1111 = 0110.
The result is a representation of the bit pattern 1001, with position mask 1111.
This operation will not work for bit patterns where a one is allowed to be in the sampe position of both masks.
