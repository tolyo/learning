#include <stdio.h>

int main()
{
    int lower(int c);
    printf("Total %c\n", lower('A'));
    printf("Total %c\n", lower('Z'));
    printf("Total %c\n", lower('a'));
    printf("Total %c\n", lower('z'));
}

int lower(int c) 
{
    return (c >= 'A' && c <= 'Z') ? c + 'a' - 'A' : c;
}