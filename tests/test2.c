/*
 * Fonction main.
 */
int main(int argc, char* argv[]) {
    //Début de la fonction main.
    puts("Hello world!"); //Afficher du texte à l’écran.
    puts("Hello\
            World!");
    printf("%d\n", 12);
    printf("%d, %d: %d\n", 42, 24, 12);
    printf("%c", 'a');
    printf("%c", '\\');
    printf("%c\n", '\'');
    printf("%c", 'a');
    printf("%c", '\t');
    printf("%c", 'b');
    printf("%c", '\n');
    puts("Helllo\tWorld!\"Bonjour\n\\");
    printf("%c%c%c%c\n", 'a', 'c', '\b', 'b');
    printf("ac\bb\n");
    printf("%d\n", -42);
    printf("%f\n", -3.141592);
    return 0;
}
