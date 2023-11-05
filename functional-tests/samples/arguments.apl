@isSameStr(string a, string b) bool {
    int counter = 0;
    for c in a {
        if (c != (b[counter])) {
            return false;
        }
        counter ++;
    }
    return true;
}

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

@main([string] argv) int {
    string one = argv[0];
    string two = argv[1];
    string three = argv[2];
    string four = argv[3];
    string five = argv[4];
    if (@isSameStr(one, "true")) && ((@strToInt(two)) == 2) {
        if (@isSameStr(three, "3.0")) && ((@strToInt(two)) == 4) {
            if (@isSameStr(five, "icanonlycountofour")) {
                return len(argv);
            }
        }
    }
    return(0);
}