#include <stdio.h>

/* Program that counts blanks tabs and newlines. Press Ctrl+D to quit prompt */
int main() {
    int blanks = 0, tabs = 0, newlines = 0, c;

    while ((c = getchar()) != EOF) {
        if (c == '\t')  ++tabs;
        if (c == ' ')   ++blanks;
        if (c == '\n')  ++newlines;
    }

    printf("blanks   %d \n", blanks);
    printf("tabs     %d \n", tabs);
    printf("newlines %d \n", newlines);
    return 0;
}