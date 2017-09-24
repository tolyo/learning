#include <stdio.h>

/* count characters in input; 1st version. To run: ./main < test */
main() {
    long nc;

    nc = 0;
    while (getchar() != EOF)
        ++nc;
    printf("%ld \n", nc);
}