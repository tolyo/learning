/*
 * Write a program detab that replaces tabs in the input with proper
 * number of blanks to space to the next tab stop. Assume a fixed
 * set of tab stops, say every n columns. Should n be a variable or a
 * symbolic parameter?
 */
#include <stdio.h>
#define N 8


int main() {
    int c, i, p = 0;

    while ((c = getchar()) != EOF) {
        if (c == '\t') {
            int lim = N - (p % N);
            for (i = 0; i < lim; ++i) {
                putchar('*');
                p++;
            }
        } else if (c == '\n') {
            putchar(c);
            p = 0;
        } else {
            putchar(c);
            p++;
        }
    }
    return 0;
}
