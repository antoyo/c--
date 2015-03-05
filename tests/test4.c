#include <stdio.h>

int main() {
    int age = 5;
    if(age >= 18) {
        puts("Vous êtes majeur.");
        if ( age > 19 ) {
            puts("Vous avez plus de 19 ans.");
        }
    }
    else {
        puts("Vous êtes mineur.");
    }
    if(age >= 18) {
        puts("Vous êtes majeur.");
    }
    else if(age > 12) {
        puts("Vous êtes un adolescent.");
    }
    else if(age < 3) {
        puts("Vous êtes un bébé.");
    }
    else {
        puts("Vous êtes mineur.");
    }
    age = 1;
    if(age >= 18) {
        puts("Vous êtes majeur.");
    }
    else if(age > 12) {
        puts("Vous êtes un adolescent.");
    }
    else if(age < 3) {
        puts("Vous êtes un bébé.");
    }
    else {
        puts("Vous êtes mineur.");
    }
    age = 15;
    if(age >= 18) {
        puts("Vous êtes majeur.");
    }
    else if(age > 12) {
        puts("Vous êtes un adolescent.");
    }
    else if(age < 3) {
        puts("Vous êtes un bébé.");
    }
    else {
        puts("Vous êtes mineur.");
    }
    age = 5;
    if(age == 18) {
        puts("Vous avez 18 ans.");
    }
    if(age == 5) {
        puts("Vous avez 5 ans.");
    }
    if(age <= 50) {
        puts("Vous avez au maximum 50 ans.");
        if(age < 30) {
            puts("Vous avez moins de 30 ans.");
        }
    }
    if(age != 18) {
        puts("Vous n’avez pas 18 ans.");
    }
    if(age < 10) {
        if(age > 5) {
            puts("Vous avez entre 5 et 10 ans.");
        }
    }
    return 0;
}
