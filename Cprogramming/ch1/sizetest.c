#include <stdio.h>

int main() {
    int a = 1432234;
    char b[2] = "11";
    printf("%zu \n" , sizeof(a));
    printf("%zu \n" , sizeof(b));

    printf("%s \n" , b);
}