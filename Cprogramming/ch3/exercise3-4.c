#include <stdio.h>
#include <string.h>
#include <limits.h>

#define NEGATIVE 0
#define POSITIVE 1

void reverse(char s[]);
void itoa(int i, char s[]);

int main() 
{
    char s[1000];
    itoa(INT_MIN, s);
    printf("%s \n", s);
    return 0;
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

void itoa(int n, char s[]) 
{
    int i, sign = POSITIVE;

    if (n < 0)  /* record sign */ {
        sign = NEGATIVE;         /* making sign positive will break for INT_MIN bc INT_MIN != INT_MAX */
    }

    i = 0;
    do {  
        /* generate digits in reverse order */
        if (sign == NEGATIVE) {
             // convert to positive
             s[i++] = -(n % 10) + '0'; /* get next digit */
        } else {
             s[i++] = n % 10 + '0'; /* get next digit */
        }
       
                
    }  while ((n /= 10) != 0);  /* delete it */  
    
    if (sign == NEGATIVE) {
        s[i++] = '-';
    }
    s[i] = '\0';    
    reverse(s);
    return;
}