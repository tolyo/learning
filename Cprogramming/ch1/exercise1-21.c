/*
 * Write a program entab that replaces strings of blanks by the minimum number of
 * tabs and blanks to achieve the same spacing. Use the same tab stops as for
 * detab. When either a tab or a single blank would suffice to react a tab stop,
 * which should be given preference? A blank of course!
 */
#include <stdio.h>
#define N 4

int main() {
    int c, p = 0, t = 0;

    while ((c = getchar()) != EOF) {
        if (c == ' ') {
            t++;
            p++;
        } else {
            if (t > 0) {
                int j;
                int tabs = t / N;
                for (j = 0; j < tabs; ++j) {
                    putchar('\t');
                }
                int spaces = t % N;
                for (j = 0; j < spaces; ++j) {
                    putchar(' ');
                }
                t = 0;
                putchar(c);
            } else {
                putchar(c);
                t = 0;
                if (c == '\n') {
                    p = 0;
                } else {
                    p++;
                }
            }
        }
    }
}