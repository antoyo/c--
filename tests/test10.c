#include <stdio.h>

int main() {
    int variable = 10;
    if(variable == 10) {
        puts("variable == 10");
    }
    if(variable == 20) {
        puts("variable == 20");
    }
    if(variable != 12) {
        puts("variable != 12");
    }
    if(variable != 10) {
        puts("variable != 10");
    }
    else {
        puts("not variable != 10");
    }
    if(variable < 20) {
        puts("variable < 20");
    }
    if(variable < 2) {
        puts("variable < 2");
    }
    if(variable <= 20) {
        puts("variable <= 20");
    }
    if(variable <= 2) {
        puts("variable <= 2");
    }
    if(variable > 20) {
        puts("variable > 20");
    }
    if(variable > 2) {
        puts("variable > 2");
    }
    if(variable >= 20) {
        puts("variable >= 20");
    }
    if(variable >= 2) {
        puts("variable >= 2");
    }
    if(2 < variable) {
        puts("2 < variable");
    }
    char* string = "Hello World!";
    puts(string);
    if(string[0] == 'H') {
        puts("string[0] == 'H'");
    }
    if('H' == string[0]) {
        puts("'H' == string[0]");
    }
    if(string[0] == 'l') {
        puts("string[0] == 'l'");
    }
    return 0;
}
