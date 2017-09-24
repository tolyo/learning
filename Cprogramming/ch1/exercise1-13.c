#include <stdio.h>
#define EMPTYLINE 0
#define WORDLINE 1

/*
 * Write a program to print a histogram of the lengths of words in its input. Horizontal version
 */
int main()
{
    int c, line = EMPTYLINE;

    while ((c = getchar()) != EOF) {
        if (c == ' ' || c == '\n' || c == '\t') {
            if (line == WORDLINE) {
                putchar('\n');
                line = EMPTYLINE;
            }
        } else {
            if (line == EMPTYLINE) line = WORDLINE;
            putchar('-');
        }
    }
}
