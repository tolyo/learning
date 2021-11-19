/*
 * Write a function expand(s1, s2) that expands
 * shorthand notations like a-z in the string s
 * into the equivalent complete list abc..xyz in s2.
     * Allow for letters of either case and digits
 * and be prepared to handle cases like a-b-c\
 * and a-z0-9 and --a-z. Arrange that a leading or trailing
 * - is taken literally.
 */
#include <stdio.h>
#define MAXLINE 1000

void expand(char s1[], char s2[]);

int main() 
{

    char s1[MAXLINE] = "-a-b-cA-C1-9-";
    char s2[MAXLINE];

    expand(s1, s2);

    printf("%s\n", s2);

    return 0;
}

void expand(char s1[], char s2[])
{
    int i = 0;
    int j = 0;
    int start = 0;
    int end = 0;

    while (s1[i] != '\0') {
        // handle leading and trailing dashes
        if (s1[i] == '-') {
            s2[j] = '-';
            j++;
        } 

        if (s1[i] >= 'A' || s1[i] >= '1') {
            // begin pattern
            start = s1[i];
            i++;
            i++;
            // peek for continuation a-b-c
            if (s1[i+1] == '-' && s1[i+2] != '\0') {
                i++;
                i++;
            }
            end = s1[i];
            while (start <= end) {
                s2[j] = start;
                j++;
                start++;
            }
            
        }
        i++;
    }
    // close pattern string
    s2[j] = '\0';    
}

