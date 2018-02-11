/*
 * Write a function invert(x, p, n) that returns x with the n bits that begin at position p
 * inverted(i.e. 1 changed into 0 and vice v rersa), leaving the others unchanged
 */

#include <stdio.h>

int main() {

    unsigned invert(x, p, n)
    {
        return x & ~(~(~0 << n) << (p + 1 - n)) |
               ((~x & ~(~0 << p + 1)) >>
               (p + 1 - n) & ~(~0 << n)) << (p + 1 - n);
    }

    printf("%u \n", invert(251,3,3));
}


