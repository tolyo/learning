/*
 * Write a loop equivalent to for loop above without using
 * && or !!
 */

int main() {
    char c;
    int valid = 1;
    for (i = 0; valid; ++i) {
        if (i < lim - 1) {
            valid = 0;
        }
        if ((c = getchar()) == EOF) {
            valid = 0;
        }
        if (c=='\n') {
            valid = 0;
        }
        if (valid == 1) {
            s[i] = c;
        }
    }
}