#include <stdio.h>
#define MAXWL 20  /* Maximum length of a word */
#define MAXNO 25 /* Maximum No of words in a sentence */

/*
 * Write a program to print a histogram of the lengths of words in its input. Vertical version
 */
int main()
{
    int c, i,
        linecount = 0,
        line = EMPTYLINE,
        longestword = 0;

    // count the number of lines ignoring empty lines
    while ((c = getchar()) != EOF) {
        if (c == '\n' && line == WORDLINE) {
            ++linecount;
            //printf("Linecount: %d \n", linecount);
            line = EMPTYLINE;
        } else {
            line = WORDLINE;
        }
    }
    printf("Linecount: %d \n", linecount);

    int words[linecount],
        currentline = 0;

    // initialize array
    for (i = 0; i < linecount; ++i) {
        words[i] = 0;
    }

    // reset line
    line = EMPTYLINE;

    // populate array with word lengths
    while ((c = getchar()) != EOF) {
        if (c == '\n' && line == WORDLINE) {
            ++currentline;
            printf("currentline: %d \n", currentline);
            line = EMPTYLINE;
        } else {
            ++words[currentline];
            line = WORDLINE;
        }
    }

    for(i = 0; i < linecount; i++) {
        printf("%d, ", words[i]);
    }



//    while ((c = getchar()) != EOF) {
//        if (c == ' ' || c == '\n' || c == '\t') {
//            putchar('\n');
//        } else {
//            putchar('-');
//        }
//    }
}
