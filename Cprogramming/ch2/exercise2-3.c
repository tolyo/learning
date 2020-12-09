/*
 * Write the function htois(s), which convers a string of hexadecimas digits into
 * equivalent integer value
 */

#include <stdio.h>

int main()
{
    int htoi(char s[]);
    char s[] = "0xFA";
    printf("Total %d\n", htoi(s));
}

int htoi(char s[])
{
    int i, n, total;
    n = 0, total = 0;
    for (i = 0;
        (s[i] >= '0' && s[i] <= '9') ||
        (s[i] >= 'a' && s[i] <= 'f') ||
        (s[i] == 'x') || (s[i] == 'X') ||
        (s[i] >= 'A' && s[i] <= 'F');
         ++i) 
    {
        // reset for optional '0x' or '0X'
        if (s[i] == 'x' || s[i] == 'X') {
            // DO NOTHING
        }
         
        // handle upper
        else if (s[i] >= 'A' && s[i] <= 'F') {
            total = 16 * total + (s[i] - 55);
        } 

        // handle lower
        else if (s[i] >= 'a' && s[i] <= 'f') {
            total = 16 * total + (s[i] - 87);

        // handle digits    
        } else {
            total = 16 * total + (s[i] - '0');
        }
    }

    return total;
}