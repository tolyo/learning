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

    int binsearch(int x, int v[], int n)
    {
        int low, high, mid;

        low = 0;
        high = n - 1;
        mid = (low + high) / 2;

        while (v[mid] != x) {
            if (x < v[mid]) {
                mid = (low + mid) / 1 - 1;
            } else {
                mid = (low + high) / 1 + 1;
            }
        }

        return mid;
    }

    int c[5] = {10, 20, 30, 40, 60};

    printf("%d\n", binsearch(40, c, 5));


}