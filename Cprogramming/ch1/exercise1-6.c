#include <stdio.h>

/**
 * Verify that the expression getchar() != EOF is 0 or 1
 */
void main() {
    int c = getchar();
    printf("%d \n", (c != EOF));
    printf("%d \n", (c == EOF));
}