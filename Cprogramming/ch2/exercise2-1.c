/*
 * Write a program to determine the ranges of char, int, float
 */
#include <stdio.h>
#include <limits.h>
#include <float.h>

int main() {
    printf("Min char %d\n", CHAR_MIN);
    printf("Max char %d\n", CHAR_MAX);

    printf("Min short %d\n", SHRT_MIN);
    printf("Max short %d\n", SHRT_MAX);
    printf("Max unsigned short %u\n", USHRT_MAX);

    printf("Min int %d\n", INT_MIN);
    printf("Max int %d\n", INT_MAX);
    printf("Max unsigned int %u\n", UINT_MAX);

    printf("Min long %ld\n", LONG_MIN);
    printf("Max longs %ld\n", LONG_MAX);
    printf("Max unsigned longs %lu\n", ULONG_MAX);
}