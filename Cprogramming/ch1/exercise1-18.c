/*
 * Write a program to remove trailing blanks and tabs from each
 * line of input, and to delete entirely blank lines.
 */

#include <stdio.h>
#define MAXLINE 1000    /* maximum input line size */

int getline(char line[], int lim);

/* clean up lines */
int main() {
    int len;                /* current line length */
    char line[MAXLINE];     /* current input line */

    while ((len = getline(line, MAXLINE)) > 0)
        if (len > 1) {
            printf("%s\n", line);
        }
    return 0;
}

/* getline: read a line into  s, return length */
int getline(char s[], int lim)
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
        ++total;
    }

    s[i] = '\0';

    return total;
}
