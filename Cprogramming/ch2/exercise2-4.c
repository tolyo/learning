/*
 * Write an alternative version of squeeze(s1, s2) that deletes each character
 * in s1 that matches any character in the string s2
 */
#include <stdio.h>

int main()
{
    int sqeeze(char s[], char c[])
    {
        int i, j, k, match = 0;
        for (i = j = 0; s[i] != '\0'; i++) {

            for (k = 0; c[k] != '\0'; k++) {
                if (s[i] == c[k]) {
                    match = 1;
                    break;
                }
            }

            if (match == 0) {
                s[j++] = s[i];
            }

            match = 0;
        }
        s[j] = '\0';
    }

    char s[] = "cdfagggaacdfaa";

    sqeeze(s, "cdf");

    printf("Total %s \n", s);
}