/*
 * Write a program to print all input lines that are longer than 80
 * characters.
 */
#include <stdio.h>
#define MAXLINE 1000    /* maximum input line size */
#define MINLINE 80      /* minimum input line size */

int getLine(char line[], int lim);
void clearArray(char line[]);

/* print all lines greater than min line */
int main() {
    int len;                /* current line length */
    char line[MAXLINE];     /* current input line */

    while ((len = getLine(line, MAXLINE)) > 0)
        if (len > MINLINE) {
            printf("%s", line);
        }
    printf("%s", "\n");
    return 0;
}

/* getLine: read a line into  s, return length */
int getLine(char s[], int lim)
{
    int c, i, total = 0;

    for (i = 0; (c = getchar()) != EOF && c!='\n'; ++i) {
        if (i > lim - 1) {
            clearArray(s);
            i = 0;
        }
        s[i] = c;
        ++total;
    }

    if (c == '\n') {
        s[i] = c;
        ++i;
    }
    s[i] = '\0';
    return total;
}

/* clearArray: reinitialize array to empty state */
void clearArray(char s[]) {
    int i;
    for (i = 0; i < MAXLINE; ++i) {
        s[i] = 0; /* or '\0' also works; */
    }
}
