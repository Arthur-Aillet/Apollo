@isBetween(int inf, int sup, int c) bool {
    if ((inf < c) || (sup > c)) {
        return true;
    }
    if ((inf == c) || (sup == c)) {
        return true;
    }
    return false;
}

@strToInt([char] str) int {
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

@fibonacci(int a, int b, int stop) int {
    if stop == 0 {
        return (a + b);
    }
    return (@fibonacci(b, (a + b), (stop - 1)));
}

@main ([string] argv) int {
    string arg = argv[0];
    int i = @strToInt(arg);
    int result = @fibonacci(1, 1, i);
    return result;
}