#include <stdio.h>

int main() {

    /* bitcount: count 1 bits in x 8*/
    int bitcount(unsigned x)
    {
        int b = 0;
        while (x > 0) {
            x &= (x-1);
            b += 1;
        }

        return b;
    }

    printf("%u \n", bitcount(247));
}


