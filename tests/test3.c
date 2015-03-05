#include <stdio.h>

int main() {
    int variable = 12;
    printf("%d\n", variable);
    int v2;
    v2 = 42;
    printf("%d\n", v2);
    v2 = 24;
    printf("%d\n", v2);
    v2++;
    printf("%d\n", v2);
    const int NOMBRE = 10;
    printf("%d\n", NOMBRE);
    printf("%d\n", -variable);
    const int A = 1, B = 2, C = 3;
    printf("%d, %d, %d\n", A, B, C);
    return 0;
}
