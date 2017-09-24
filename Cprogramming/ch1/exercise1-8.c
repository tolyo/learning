#include <stdio.h>

/* program that counts blanks tabs and newlines */
main() {
    double blanks = 0.0, tabs = 0.0, newlines = 0.0, c;

    while ((c = getchar()) != EOF) {
        if (c == '\t')  ++tabs;
        if (c == ' ')   ++blanks;
        if (c == '\n')  ++newlines;
    }

    printf("blanks   %.0f \n", blanks);
    printf("tabs     %.0f \n", tabs);
    printf("newlines %.0f \n", newlines);
}