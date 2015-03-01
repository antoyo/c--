int main() {
    int variable = 10;
    int new_variable = add(variable);
    printf("%d\n", new_variable);
    printf("%d\n", add(40));
    return 0;
}

int add(int number) {
    return number + 2;
}
