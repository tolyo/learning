/*
 * Write a function any(s1, s2), which returns the first location in the string
 * s1 where any character from the string s2 occurs, or -1 if s1 contains
 * no characters from s2. (The standard library function strpbrk does the same
 * job but returns a pointer to the locations)
 */
#include <stdio.h>

int main()
{
    int any(char s1[], char s2[]);
    char s1[] = "12345x";

    printf("Index at %dth place \n", any(s1, "asdx"));
}


int any(char s1[], char s2[])
{
    int i, k = 0;
    for (i = 0; s1[i] != '\0'; i++) {

        for (k = 0; s2[k] != '\0'; k++) {
            if (s1[i] == s2[k]) {
                return ++i;
            }
        }

    }
    return -1;
}