#include <stdio.h>

/* program copies its input to its output replacing
 * each string of one or more blanks by a single blank
 */
main() {
    double blank = 0, c;

    while ((c = getchar()) != EOF) {
        if (c == ' ')   {
            if (blank == 0) {
                blank = 1;
                putchar(c);
            }
        } else {
            if (blank = 1) blank = 0;
            putchar(c);
        }
    }
}