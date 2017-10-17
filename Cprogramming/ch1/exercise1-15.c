#include <stdio.h>

/* print Fahrenheit-Celsius table
 * for fahr = 0, 20 ...., 300*/

double toCelsius(double fahr);

main()
{
    float fahr, celsius;
    int lower, upper, step;

    lower = 0;      /* lower limit */
    upper = 300;    /* upper limit */
    step  = 20;     /* step */

    fahr = lower;
    while (fahr <= upper) {
        printf("%3.0f  %6.1f \n", fahr, toCelsius(fahr));
        fahr = fahr + step;
    }
}

double toCelsius(double fahr) {
    return 5.0 * (fahr - 32.0) / 9.0;
}


