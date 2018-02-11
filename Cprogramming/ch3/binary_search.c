
#include <stdio.h>

int main()
{

    int binsearch(int x, int v[], int n)
    {
        int low, high, mid;

        low = 0;
        high = n - 1;

        while (low <= high) {
            mid = (low + high) / 2;
            if (x < v[mid]) {
                high = mid - 1;
            } else if (x > v[mid]) {
                low = mid + 1;
            } else {
                return mid;
            }
        }

        return -1;
    }

    int c[5] = {10, 20, 30, 40, 60};

    printf("%d\n", binsearch(40, c, 5));

}