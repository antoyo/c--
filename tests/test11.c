int main() {
    char* string = "hello world!";
    putc(string[0]);
    puts("");

    char* otherString = string;
    putc(otherString[0]);
    puts("");
    return 0;
}
