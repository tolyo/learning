/*
 * Write the function htois(s), which convers a tring of hexadecimas digits into
 * equivalent integer value
 */

#include <stdio.h>

int main()
{
    int htoi(char s[])
    {
        int i, n, total;
        n = 0, total = 0;
        for (i = 0;
             (s[i] >= '0' && s[i] <= '9') ||
             (s[i] >= 'a' && s[i] <= 'f') ||
             (s[i] == 'x') || (s[i] == 'X') ||
             (s[i] >= 'A' && s[i] <= 'F'); ++i) {
            // reset for optional '0x' or '0X'
            if (s[i] == 'x' || s[i] == 'X') {
            } else if (s[i] >= 'A' && s[i] <= 'F') {
                total = 16 * total + (s[i] - 55);
            } else if (s[i] >= 'a' && s[i] <= 'f') {
                total = 16 * total + (s[i] - 87);
            } else {
                total = 16 * total + (s[i] - '0');
            }
        }
        return total;
    }
    char s[] = "0XAF";
    printf("Total %d\n", htoi(s));
}