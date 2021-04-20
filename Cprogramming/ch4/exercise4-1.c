/*
 *  Write the function strindex(s, t), which returns the position
 *  of the rightmost occurence of t in s, or -1  if there is none
 */
#include <stdio.h>

#define MAXLINE 1000 /* maximum input line length */

int getLine(char line[], int max);
int strindex(char source[], char searchfor[]);

char pattern[] = "abc"; /* pattern for search for */

/* find all lines matching pattern */
int main() 
{
    char line[MAXLINE];
    int found = 0;

    while (getLine(line, MAXLINE) > 0) 
        if (strindex(line, pattern) >= 0) {
            printf("%s", line);
            found++;
        }

    return found;
}


/* getLine: get line into s, return length */
int getLine(char s[], int lim)
{
    int c, i;
    i = 0;
    while (--lim > 0 && (c=getchar()) != EOF && c != '\n')
        s[i++] = c;
    if (c == '\n')
        s[i++] = c;
    s[i] = '\0';
    return i;        
}

/* strindex: return index of t in s, -1 if none */
int strindex(char s[], char t[])
{
    int i, j, k, result;

    result = -1;

    for(i=0;s[i]!='\0';i++)
    {
        for(j=i,k=0;t[k]!='\0' && s[j]==t[k];j++,k++)
            ;
        if(k>0 && t[k] == '\0')
            result = i;
    }   
    printf("result %d", result);
    return result;
}
