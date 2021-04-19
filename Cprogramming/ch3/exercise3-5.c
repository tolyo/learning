/*
 *  Write the function itob(n,s,b) that converts the integer n
 *  into a base b character representation in the string s. In
 *  particular, itob(n,s,16) formats n as a hexadecimal integer 
 *  in s.
 */

#include <stdio.h>
#include <string.h>

void itob(int n, char s[], int b);
void reverse(char s[]);

int main() 
{
    char s[100];
    itob(42425, s, 16);
    printf(" %s", s);
    return 0;
}

void itob(int n, char s[], int b)
{
    int i,j,sign;
    
    i = 0;

    if((sign=n)<0)
        n = -n;

    do {

        j = n % b;
        s[i++] = (j <= 9) ? j + '0' : j + 'a'- 10;
    } while ((n /= b) != 0);    

    if(sign < 0)
        s[i++]='-';

    reverse(s);
    return;
}

void reverse(char s[]) 
{
    int c, i, j;

    for (i = 0, j = strlen(s) - 1; i < j; i++, j--) {
        c = s[i];
        s[i] = s[j];
        s[j] = c;
    }
}