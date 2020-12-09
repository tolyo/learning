#include <stdio.h>
#define EMPTYLINE 0
#define WORDLINE 1
#define MAX_WORD_COUNT 100

/*
 * Write a program to print a histogram of the lengths of words in its input. Vertical version
 */
int main()
{
    int c, line = EMPTYLINE;
    int words[MAX_WORD_COUNT];
    int current_word = 0;
    int max_word_length = 0;

    // initialize array
    for (int i = 0; i < MAX_WORD_COUNT; i++)
    {
        words[i] = 0;
    };
    

    while ((c = getchar()) != EOF) {
        if (c == ' ' || c == '\n' || c == '\t') {
            if (line == WORDLINE) {
                ++current_word;
                line = EMPTYLINE;
            }
        } else {
            if (line == EMPTYLINE) line = WORDLINE;
            ++words[current_word];

            if (words[current_word] > max_word_length) {
                max_word_length = words[current_word];
            }
        }
    }

    for (int z = 1; z <= max_word_length; z++) {
        for (int y = 0; y < current_word; y++) {
            if (words[y] >= z) {
                putchar('|');
            } else {
                // without this the histogram will 'float' to the left
                putchar(' ');
            }
        }
        putchar('\n');
    }
}
