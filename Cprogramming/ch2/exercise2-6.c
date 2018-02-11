/*
 * Write a function setbits(x, p, n, y) that returns x with the n bits that begin
 * at position p set to the rightmonst n bits y, leaving the other bits unchanged
 */
#include <stdio.h>

int main() {
    unsigned setbits(x, p, n, y)
    {
        return x & ~(~(~0 << n) << (p+1-n)) | ( y & ~(~0<<n)) << (p+1-n);
    }

    printf("%u \n",setbits(255,3,4,1));
}


