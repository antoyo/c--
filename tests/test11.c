#include <stdio.h>

int main() {
    char* string = "hello world!";
    printf("%c\n", string[0]);

    char* otherString = string;
    printf("%c\n", otherString[0]);
    return 0;
}
