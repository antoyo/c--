#include <stdio.h>

int main() {
    int age = 5;
    int majeur = age >= 18 ? 1 : 0;
    printf("%d\n", majeur);
    age = 20;
    majeur = age >= 18 ? 1 : 0;
    printf("%d\n", majeur);
    int entre30et50 = age < 30 ? 0 : age > 50 ? 0 : 1;
    printf("%d\n", entre30et50);
    age = 35;
    entre30et50 = age < 30 ? 0 : age > 50 ? 0 : 1;
    printf("%d\n", entre30et50);
    return 0;
}
