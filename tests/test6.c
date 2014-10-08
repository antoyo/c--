int max(int nombre1, int nombre2) {
    if(nombre1 > nombre2) {
        return nombre1;
    }
    else {
        return nombre2;
    }
}

int number() {
    return 24;
}

int main() {
    int my_number = number();
    puti(my_number);
    puts("");
    int nombre = max(42, 5);
    puti(nombre);
    puts("");
    nombre = max(42, 50);
    puti(nombre);
    puts("");
    return 0;
}
