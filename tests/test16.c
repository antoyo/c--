#include <stdio.h>

int main() {
    int nombre = 10 + 2 * 3;
    printf("%d\n", nombre);
    nombre = 100 / 10 - 2 * 10;
    printf("%d\n", nombre);
    nombre = 100 / 10 - 2 * 10 + 10 / 2;
    printf("%d\n", nombre);
    nombre = 100 / 10 - 2 * 10 + 10 / 2 + 17 % 5;
    printf("%d\n", nombre);
    printf("%d\n", 10 / 2 + 2 * 5);
    printf("%d\n", 10 / 2 - 2 * 5);
    printf("%d\n", 10 - 2 - 5);

    if(nombre + 40 < 50 - 10) {
        puts("yes");
    }
    return 0;
}
