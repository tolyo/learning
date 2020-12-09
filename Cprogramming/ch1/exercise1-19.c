/*
 * Write a function reverse(s) that reverse
 * the character sting s. Use it to write a program
 * that reverse its input a line at a time.
 */

#include <stdio.h>
#define MAXLINE 1000    /* maximum input line size */

int getLine(char line[], int lim);
void reverse(char line[]);

/* clean up lines */
int main() {
    int len;                /* current line length */
    char line[MAXLINE];     /* current input line */

    while ((len = getLine(line, MAXLINE)) > 0)
        if (len > 1) {
            reverse(line);
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

    s[i] = '\0';

    return total;
}

/* reverse: reverse line */
void reverse(char line[]) {
    int i, j;
    char a;

    /* find length */
    for (i = 0; line[i] != '\0'; ++i) {}
    /* decrease by by one */
    --i;

    /* reverse one har at a time */
    for (j = 0; j <= i; ++j) {
        a = line[j];
        line[j] = line[i];
        line[i] = a;
        --i;
    }

}
