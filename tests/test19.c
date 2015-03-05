#include <stdio.h>

int main() {
    int a;
    int b;
    int c;
    a = 1, b = 2;
    printf("%d, %d\n", a, b);
    a = 42, b = 24, c = 12;
    printf("%d, %d, %d\n", a, b, c);
    int x, y, z;
    x = 3, y = 4, z = 5;
    printf("%d, %d, %d\n", x, y, z);
    int d = 6, e = 7, f = 8, g = 9;
    printf("%d, %d, %d, %d\n", d, e, f, g);
    int h;
    h = a, b;
    printf("%d\n", h);
    h = (a, b);
    printf("%d\n", h);
    for(int i = 0, j = 5 ; i < j ; i++, j--) {
        printf("%d, %d\n", i, j);
    }
    return 0;
}
