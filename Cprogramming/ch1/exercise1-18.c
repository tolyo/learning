/*
 * Write a program to remove trailing blanks and tabs from each
 * line of input, and to delete entirely blank lines.
 */

#include <stdio.h>
#define MAXLINE 1000    /* maximum input line size */

int getLine(char line[], int lim);

/* clean up lines */
int main() {
    int len;                /* current line length */
    char line[MAXLINE];     /* current input line */

    while ((len = getLine(line, MAXLINE)) > 0)
        if (len > 1) {
            printf("%s\n", line);
        }
    return 0;
}

/* getLine: read a line into  s, return length */
int getLine(char s[], int lim)
{
    int c, i, j, total = 0;

    /* clear array */
    for (i = 0; i < lim; i++) {
        s[i] = 0;
    }

    for (i = 0; i < lim - 1 && (c = getchar()) != EOF && c!='\n'; ++i) {
        s[i] = c;
        ++total;
    }

    /* work backwards on the array removing blanks and tabs */
    for (j = i - 1; (s[j] == ' ' || s[j] == '\t'); --j) {
        s[j] = 0;
        --total;
    }

    if (c == '\n') {
        s[i] = c;
        ++i;
    }

    s[i] = '\0';

    return total;
}
