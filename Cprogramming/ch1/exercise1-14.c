/*
 * Program to print a histogram of the frequencies of different characters in its input
 */

#include <stdio.h>

int main()
{
    int c, i, j;
    int ascii[128];

    // populate array with ascii printable chars
    for (j = 0; j < 128; ++j) {
        ascii[j] = 0;
    }

    // increase char count in array for each char input
    while ((c = getchar()) != EOF) {
        if (c >= 32) {
            ++ascii[c];
        }
    }

    // print histogram
    for (i = 32; i < 127; ++i) {
        printf("%c: ", i);
        for (j = 0; j < ascii[i]; ++j) {
            printf("-");
        }
        printf("\n");
    }
}