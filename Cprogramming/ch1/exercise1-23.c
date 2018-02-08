/*
 * Write a program to remove all comments from a C program
 * Don't forget to handle quotes strings and character constants properly
 * C comments do not nest
 * In the C language, comments are enclosed between two sets of characters
 * or in multiline
 */


#include<stdio.h>
#define MAXLINE 50

int commentStarted = 0;

char line[MAXLINE];                 // current line
int pos = 0;
int iscommentstart(int pos);        // The Boolean value true is coded as 1, and false is coded as 0
int iscommentend(int pos);          //
int printlineTo(int pos);           // Print line up to ending position
int printlineFrom(int pos);         // Print line from starting position
void printline();

int main()
{
    int c; // char
    while (c = getchar()) {
        //printf('%d\n', c);
        if (c == EOF) {
            if (commentStarted == 0) {
                printline();
            }
            return;
        }
        // if encounter new line then print everything we have up to this point
        else if (c == '\n' && commentStarted == 0) {
            line[pos] = c;
            printline();
            pos = 0;
        } else {
            line[pos] = c;
            if (iscommentstart(pos) == 1) {
                commentStarted = 1;
                printlineTo(pos - 1);
                ++pos;
            } else if (commentStarted == 1) {
                if (iscommentend(pos) == 1) {
                    commentStarted = 0;
                    pos = 0;
                }
            } else {
                ++pos;
            }

        }
    }
    return 0;
}

int printlineTo(int pos)
{
    if (pos == 0)
        return;
    int i;
    for (i = 0; i < pos; ++i) {
        putchar(line[i]);
    }
}

int printlineFrom(int pos)
{
    int i;
    for (i = pos; (line[i] != '\n') || (line[i] != EOF); ++i) {
        putchar(line[i]);
    }
    putchar('\n');
}

void clearArray()
{
    int i;
    for (i = 0; i < MAXLINE - 1; ++i) {
        line[i] = 0; /* or '\0' also works; */
    }
}

void printline()
{
    int i;
    for (i = 0; i < pos; ++i) {
        putchar(line[i]);
    }
    putchar('\n');
}

int iscommentstart(int pos)
{
    if (line[pos] == '*' && line[pos - 1] == '/') {
        return 1;
    } else {
        return 0;
    }
}


int iscommentend(int pos)
{
    if (line[pos-1] == '*' && line[pos] == '/') {
        return 1;
    } else {
        return 0;
    }
}