int main() {
    int age = 5;
    if(age >= 18) {
        puts("Vous êtes majeur.\n");
        if ( age > 19 ) {
            puts("Vous avez plus de 19 ans.\n");
        }
    }
    else {
        puts("Vous êtes mineur.\n");
    }
    if(age == 18) {
        puts("Vous avez 18 ans.\n");
    }
    if(age == 5) {
        puts("Vous avez 5 ans.\n");
    }
    if(age <= 50) {
        puts("Vous avez au maximum 50 ans.\n");
        if(age < 30) {
            puts("Vous avez moins de 30 ans.\n");
        }
    }
    if(age != 18) {
        puts("Vous n’avez pas 18 ans.\n");
    }
    if(age < 10) {
        if(age > 5) {
            puts("Vous avez entre 5 et 10 ans.\n");
        }
    }
    return 0;
}
