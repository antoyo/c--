int main() {
    int i = 0;
    while(i < 5) {
        puti(i);
        puts("");
        i++;
    }
    char letter = 'a';
    while(letter <= 'z') {
        putc(letter);
        letter++;
    }
    puts("");
    return 0;
}
