int main() {
    int result = 0;
    if(result == 0) {
        puts("result == 0");
    }
    result = 5 + 3;
    if(result == 8) {
        puts("result == 8");
    }
    if(result == 9) {
        puts("result == 9");
    }
    result = 17 - 2;
    if(15 == result) {
        puts("result == 15");
    }
    if(16 == result) {
        puts("result == 16");
    }
    result = 2 * 25;
    if(60 == result) {
        puts("result == 60");
    }
    if(50 == result) {
        puts("result == 50");
    }
    result = 100 / 4;
    if(25 == result) {
        puts("result == 25");
    }
    result = 17 % 5;
    if(result == 2) {
        puts("result == 2");
    }
    int a = 2;
    int b = 3;
    int c = a * b;
    if(c == 6) {
        puts("c == 6");
    }
    c = c + 1;
    if(c == 7) {
        puts("c == 7");
    }
    c = 1 + c;
    if(c == 8) {
        puts("c == 8");
    }
    c++;
    if(c == 9) {
        puts("c == 9");
    }
    c = c - 5;
    if(c == 4) {
        puts("c == 4");
    }
    c--;
    if(c == 3) {
        puts("c == 3");
    }
    c += 10;
    if(c == 13) {
        puts("c == 13");
    }
    c += a;
    if(c == 15) {
        puts("c == 15");
    }
    c -= 3;
    if(c == 12) {
        puts("c == 12");
    }
    c *= 3;
    if(c == 36) {
        puts("c == 36");
    }
    c /= 2;
    if(c == 18) {
        puts("c == 18");
    }
    c %= 5;
    if(c == 3) {
        puts("c == 3");
    }
    return 0;
}
