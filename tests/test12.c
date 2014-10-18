int main() {
    int variable = 10;
    int new_variable = add(variable);
    puti(new_variable);
    puts("");
    puti(add(40));
    puts("");
    return 0;
}

int add(int number) {
    return number + 2;
}
