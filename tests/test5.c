#include <stdio.h>

int main() {
    int i = 0;
    while(i < 5) {
        printf("%d\n", i);
        i++;
    }
    char letter = 'a';
    while(letter <= 'z') {
        printf("%c", letter);
        letter++;
    }
    puts("");
    return 0;
}
