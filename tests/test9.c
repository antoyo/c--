#include <stdio.h>

int main() {
    int i = 0;
    while(i < 5) {
        puts("while");
        i++;
    }
    i = 0;
    do {
        puts("do while");
        i++;
    } while(i < 10);
    i = 0;
    while(i < 5) {
        puts("while");
        i++;
    }
    for(int j = 0 ; j < 3 ; j++) {
        puts("for");
    }
    return 0;
}
