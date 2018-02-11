#include <stdio.h>

int main()
{
    unsigned getbits(unsigned x, int p, int n)
    {
        return (x >> (p+1-n)) & ~(~0 << n);
    }

    printf("%d\n", getbits(0xFF94, 4, 3));
}