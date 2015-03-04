#include <stdio.h>

int max(int nombre1, int nombre2) {
    if(nombre1 > nombre2) {
        return nombre1;
    }
    else {
        return nombre2;
    }
}

int number() {
    return 24;
}

int main() {
    int my_number = number();
    printf("%d\n", my_number);
    int nombre = max(42, 5);
    printf("%d\n", nombre);
    nombre = max(42, 50);
    printf("%d\n", nombre);
    return 0;
}
