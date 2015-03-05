#include <stdio.h>

int main() {
    if(2 == 2 == 1) {
        puts("2");
    }
    if(2 == 2 == 2) {
        puts("2");
    }
    if(2 < 3 < 2) {
        puts("vrai");
    }
    if(2 < 3 < 1) {
        puts("faux");
    }
    if(2 < 3 < 2 < 2) {
        puts("vrai");
    }
    return 0;
}
