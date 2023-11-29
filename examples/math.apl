@intToStr(int i) string {
    string result = "";
    int number = i;
    int temp = 0;
    while (number != 0) {
        temp = number % 10;
        result = [('0' + temp as char )] : result ;
        number /= 10;
    }
    if ((len(result)) == 0) {
        result = "0";
    }
    return result;
}

@main([string] argv) int {

    int i = 1;
    i ++;
    i += 8;
    i /= 2;
    i *= 5;
    i %= 4;
    i = (i + 9);
    i = (i - -15);
    i = (i * 2);
    i = (i / 2);
    print((@intToStr(i)));
    print("\n");
    return (i);
}