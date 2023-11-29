@isBetween(int inf, int sup, int c) bool {
    if ((inf < c) || (sup > c)) {
        return true;
    }
    if ((inf == c) || (sup == c)) {
        return true;
    }
    return false;
}

@strToInt(string str) int {
    int result = 0;
    int iterator = 0;
    while ((len(str)) != iterator) {
        if (@isBetween('0' as int, '9' as int, (str[iterator]) as int)) {
            result += ((str[iterator]) as int) - 48;
            result *= 10;
            iterator += 1;
        }
    }
    result /= 10;
    return result;
}

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

@main ([string] args) int {
    if ((len(argv)) < 1) {
        print("invalid arguments\: needs a number\n");
        return (0);
    }
    int result = 500;
    string arg = args[0];
    result = @strToInt(arg);
    print((@intToStr(result)));
    print("\n");
    return (result);
}