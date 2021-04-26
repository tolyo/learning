/*
	Extend atof to handle scientific notation.
*/ 

#include <stdio.h>
#include <ctype.h>

# define MAXLINE 100

int getLine(char line[], int max);
int isDigit(char c);

/* rudimentary calculator */
int main()
{
	double sum, atof(char[]);
	char line[MAXLINE];
	

	sum = 0;
	while (getLine(line, MAXLINE) > 0)
		printf("\t%g\n", sum += atof(line));
		
	return 0;
}

/* atof: convert string s to double */
double atof(char s[])
{
	double val, power;
	int i, sign, scientific_sign, scientific = 0, scientific_val, scientific_total = 1;

	for (i = 0; isspace(s[i]); i++)  /* skip white space */
		;

	sign = (s[i] == '-') ? -1 : 1;
	if (s[i] == '+' || s[i] == '-')
		i++;

	for (val = 0.0; isdigit(s[i]); i++)
		val = 10.0 * val + (s[i] - '0');
	
	if (s[i] == '.')
		i++;
	for (power = 1.0; isdigit(s[i]); i++) {
		val = 10.0 * val + (s[i] - '0');
		power *= 10.0;
	}		

	if (s[i] == 'e' || s[i] == 'E')
	{
		scientific = 1;
		i++;
	}

	if (scientific)
	{
		scientific_sign = (s[i] == '-') ? -1 : 1;
		if (s[i] == '+' || s[i] == '-')
			i++;
	 	for (scientific_val = 0; isdigit(s[i]); i++)
	 		scientific_val = 10 * scientific_val + (s[i] - '0');

	 	for (;scientific_val > 0; scientific_val--)
		 	scientific_total *= 10;

		if (scientific_sign < 0)
		{
			return (sign * val / power) / scientific_total;
		} else {
			return (sign * val / power) * scientific_total;
		}
	}

	return sign * val / power;	

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
