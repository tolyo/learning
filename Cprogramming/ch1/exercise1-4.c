#include <stdio.h>

/* print Fahrenheit-Celsius table
 * for fahr = 0, 20 ...., 300
 * with table headings
 */

main()
{
    float fahr, celsius;
    int lower, upper, step;

    lower = 0;      /* lower limit */
    upper = 300;    /* upper limit */
    step  = 20;     /* step */

    celsius = lower;
    printf("%s %s\n", "Celsius", "Fahrenheit");
    while (fahr <= upper) {
        fahr = 9.0 / 5.0 * celsius + 32.0;
        printf("%8.0f  %6.1f \n", celsius, fahr);
        celsius = celsius + step;
    }
}
