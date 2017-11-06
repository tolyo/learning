/*
 * Write a program entab that replaces strings of blanks by the minimum number of
 * tabs and blanks to achieve the same spacing. Use the same tab stops as for
 * detab. When either a tab or a single blank would suffice to react a tab stop,
 * which should be given preference? A blank of course!
 */
#include <stdio.h>
#define N 8

int main() {
    int c, p = 0, sps = 0, tabs = 0;

    while ((c = getchar()) != EOF) {
        if (c == ' ') {
            p++;
            if (p % N == 0) {
                tabs++;
                if (sps = N) {
                    sps=sps - N;
                }
            } else {
                sps++;
            }
        } else {
            if (tabs > 0 || sps > 0) {
                for (tabs; tabs != 0; tabs--) {
                    putchar('\t');
                }
                for (sps; sps != 0; sps--) {
                    putchar(' ');
                }
                putchar(c);
            } else {
                putchar(c);
                if (c == '\n') {
                    p = 0;
                } else {
                    p++;
                }
            }
        }
    }
}