#include <stdio.h>

/**
 * Verify that the expression getchar() != EOF is 0 or 1
 */
void main() {
    int c = getchar();
    printf("3 %d \n", (c != EOF));
    printf("%d \n", (c == EOF));
}