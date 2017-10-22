/*
 * Revise the main routine of the longest-line program so it will correctly
 * print the length of the arbitrarily long input lines, and as much as
 * possible of the text
 */

#include <stdio.h>
#define MAXLINE 10 /* maximum input line size */

int getline(char line[], int lim);
void copy(char to[], char from[]);
void clearArray(char line[]);

/* print the longest input line */
main() {
    int len;    /* current line length */
    int max;    /* maximum length seen so far */
    char line[MAXLINE];     /* current input line */
    char longest[MAXLINE];  /* longest line saved here */

    max = 0;
    while ((len = getline(line, MAXLINE)) > 0)
        if (len > max) {
            max = len;
            copy(longest, line);
        }

    if (max > 0) /* there was a line */
        printf("Length: %d \n", max);
        printf("%s \n", longest);

    return 0;
}

/* getline: read a line into  s, return length */
int getline(char s[], int lim)
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
        ++total;
    }
    s[i] = '\0';
    return total;
}

/* clearArray: reinitialize array to empty state */
void clearArray(char s[]) {
    int i;
    for (i = 0; i < MAXLINE - 1; ++i) {
        s[i] = 0; /* or '\0' also works; */
    }
}

/* copy: copy 'from' into 'to'; assume to is big enoguh */
void copy(char to[], char from[])
{
    int i;
    i = 0;
    while ((to[i] = from[i]) != '\0')
        ++i;
}