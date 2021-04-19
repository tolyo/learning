/*
 * Write a function rightrot(x, n) that returns  the value of the
 * integer x rotated to the right by n bit positions.
 * */
#include <stdio.h>

unsigned rightrot(x, n);

int main() {
    printf("%u \n",rightrot(240,3));
}


unsigned rightrot(x, n)
{
    return ((x & (~0 << n)) >> n) | ((x & (~(~0 << n))) << n);
}

