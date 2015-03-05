#include <stdio.h>

int main() {
    int age = 5;
    int majeur = age >= 18 ? 1 : 0;
    printf("%d\n", majeur);
    age = 20;
    majeur = age >= 18 ? 1 : 0;
    printf("%d\n", majeur);
    return 0;
}
