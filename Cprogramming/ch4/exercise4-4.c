/**
 * Add commands to print the top element of the status
 * without poping, to dublicate it, and to swap the top two elemment.
 * Add a command to clear the stack 
 */

#include <stdio.h>
#include <stdlib.h> /* for atof() */
#include <ctype.h>

#define MAXOP 100 /* max size of operand or operator */
#define NUMBER '0' /* signal that a number was found */

int getop(char []);
void push(double);
void printTop();
void clearStack();
double pop(void);

/* reverse Polish calculator */
int main() 
{
    int type;
    double op1;
    double op2;
    char s[MAXOP];

    while ((type = getop(s)) != EOF)
    {
        switch (type) {
            case NUMBER:
                push(atof(s));
                break;
            case '+':
                push(pop() + pop());
                break;
            case '*':
                push(pop() * pop());
                break;
            case '-':
                op2 = pop();                
                push(pop() - op2);
                break;
            case '/':
                op2 = pop();
                if (op2 != 0.0)
                    push(pop() / op2);
                else 
                    printf("error: zero divisor \n");
                break;
            case '%':
                op2 = pop();
                push((int)pop() % (int)op2);
                break;
            case '?':
                printTop();
                break;      
            case 'c':
                clearStack();
                break;
            case 'd':
                op2 = pop();
                push(op2);
                push(op2);
                break;    
            case 's':
                op1 = pop();
                op2 = pop();
                push(op1);
                push(op2);
                break;    
            case '\n':
                printf("Result \t%.8g\n", pop());    
                break;
            default:
                printf("error: unknow command %s\n", s);
                break;    
        }
    }
    return 0;    
}

#define MAXVAL 100 /* maximum depth of val stack */

int sp = 0;             /* next free stack position */
double val[MAXVAL];     /* value stack */

/* push: push f onto value stack */
void push(double f)
{
    if (sp < MAXVAL)
        val[sp++] = f;
    else 
        printf("error: stack full, can't push %g\n", f);    
}

/* pop: pop and return top value from stack */
double pop(void)
{
    if (sp > 0)
        return val[--sp];
    else {
        printf("error: stack empty\n");
        return 0.0;
    }
}

int getch(void);
void ungetch(int);

/* getop: get next operator or numeric operand */
int getop(char s[])
{
    int i, c, sign = 0;
    while ((s[0] = c = getch()) == ' ' || c == '\t')
        ;
    s[1] = '\0';
    
    if(!isdigit(c) && c != '.' && c != '-') // not a number
        return c;

    i = 0;

    // handle negative numbers
    if (c == '-')
    {
        if(isdigit(c = getch()) || c == '.')
            s[++i] = c; //negative numbers
        else
        {
            return '-';
        }
    }
    
    if (isdigit(c)) /* collect the integer part */
        while (isdigit(s[++i] = c = getch()))
            ;
    
    if (c == '.')   /* collect fraction part */
        while (c == '-' || isdigit(s[++i] = c = getch()))
            ;
            
    s[i] = '\0';
    
    if (c != EOF)
        ungetch(c);
        
    return NUMBER;
}

#define BUFSIZE 100

char buf[BUFSIZE];  /* buffer for ungetch */
int bufp = 0;       /* next free position in buf */

int getch(void) /* get a possibly pushed back character */
{
    return (bufp > 0) ? buf[--bufp] : getchar();
}

void ungetch(int c)  /* push character back on input */
{
    if (bufp >= BUFSIZE)
        printf("ungetch: too many characters\n");
    else 
        buf[bufp++] = c;    
}

/* print the top elemen tof the stack */
void printTop() 
{
    printf(" %d  %c \n", buf[0], buf[0]);
}

void clearStack()
{
    bufp = 0;
}

