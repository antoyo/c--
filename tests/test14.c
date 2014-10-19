int main() {
    int variable = 5;
    switch(variable) {
        case 0:
            puts("Zero");
            break;
        case 5:
            puts("Five");
            break;
        case 10:
            puts("Ten");
            break;
        default:
            puts("Other");
    }

    switch(variable) {
        case 0:
            puts("0");
            break;
        case 100:
            puts("100");
            break;
        case 1000:
            puts("1000");
            break;
        default:
            puts("x");
    }

    switch(variable) {
        case 0:
            puts("Zero");
        case 1:
            puts("One");
        case 2:
            puts("Two");
    }

    switch(variable) {
        case 1:
        case 2:
        case 3:
            puts("[1-3]");
            break;
        case 4:
        case 5:
        case 6:
            puts("[4-6]");
            break;
    }
    return 0;
}
