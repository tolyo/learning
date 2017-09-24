#include <stdio.h>

/* program copies its input to its output replacing
 * each string of one or more blanks by a single blank
 */
main() {

    int blank = 0; // blank keeps the state for current one or more blanks
    int c;

    while ((c = getchar()) != EOF) {
        if (c == ' ')   {
            if (blank == 0) {
                // print blank only if no previous blanks were encountered
                blank = 1;
                putchar(c);
            }
            // ignore rest of blanks
        } else {
            // reset blank to 0 if not blank
            if (blank = 1) blank = 0;
            putchar(c);
        }
    }
}