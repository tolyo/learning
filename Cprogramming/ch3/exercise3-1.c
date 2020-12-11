/*
 * Our binary search makes two tests inside the loop,
 * when one would suffice (at the price of more tests
 * outside).
 * Write a binary search with only one test inside
 * the loop and measure the difference in run-time.
 */

#include <stdio.h>

int main()
{

    int binsearch(int x, int v[], int n);

    int c[10] = {10, 20, 30, 40, 44, 45, 46, 47, 57, 60};

    printf("%d\n", binsearch(47, c, 10));
    printf("%d\n", binsearch(10, c, 10));
    printf("%d\n", binsearch(1, c, 10));
}

int binsearch(int x, int v[], int n)
{
    int low, high, mid, count;

    low = 0;
    high = n - 1;
    mid = (low + high) / 2;

    while (v[mid] != x && low != high) {
        if (x < v[mid]) {
            high = mid - 1;
        } else {
           low = mid + 1;
        }        
        mid = (low + high) / 2;
    }

    if (low == high && v[mid] != x) {
        return -1;
    } else {
        return mid;
    }
}