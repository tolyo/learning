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

    fahr = lower;
    printf("%s %s\n", "Fahrenheit", "Celsius");
    while (fahr <= upper) {
        celsius = 5.0 * (fahr - 32.0) / 9.0;
        printf("%8.0f  %6.1f \n", fahr, celsius);
        fahr = fahr + step;
    }
}
