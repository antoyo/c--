#include <stdio.h>

int main() {
    int age = 35;
    if(age > 30 && age < 50) {
        puts("Vous avez entre 30 et 50 ans.");
    }
    if(age < 18 || age > 30) {
        puts("Soit vous êtes mineur, soit vous avez plus de 30 ans.");
    }
    if(age < 18 || age > 60) {
        puts("Soit vous êtes mineur, soit vous avez plus de 60 ans.");
    }
    if(age < 18 && age > 30) {
        puts("Vous avez moins de 18 ans et plus de 30 ans.");
    }
    if(age < 18 || age > 20 && age > 30) {
        puts("Soit vous êtes mineur, soit vous avez plus de 30 ans.");
    }
    if(age > 30 && (age > 60 || age < 50)) {
        puts("Soit vous avez entre 30 et 50 ans, soit vous avez plus de 60 ans.");
    }
    if(!(age < 18)) {
        puts("Vous n’êtes pas mineur.");
    }
    if(age < 18 || age > 50 || age < 40) {
        puts("Soit vous êtes mineur, soit vous avez plus de 50 ans, soit vous avez moins de 40 ans.");
    }
    if(age > 18 && age < 50 && age < 40) {
        puts("Vous avez entre 18 et 40 ans.");
    }
    if(!!(age < 18)) {
        puts("Vous êtes mineur.");
    }
    if(!!!(age < 18)) {
        puts("Vous n’êtes pas mineur.");
    }
    return 0;
}
