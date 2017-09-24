#include <stdio.h>

#define IN  1   /* inside a word */
#define OUT 0   /* outside a word */

/* prints its input one word per line */
main() {
    int c, state;
    state = OUT;

    while ((c = getchar()) != EOF) {
        if (c != ' ' && c != '\t') {
            state = IN;
            putchar(c);
        } else {
            if (state != OUT) putchar('\n');
            state = OUT;
        }
    }

}