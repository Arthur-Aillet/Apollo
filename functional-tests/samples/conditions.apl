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

@main([string] argv) int {
    string arg = argv[0];
    int result = 0;
    while (result < 5) {
        result ++;
    }
    if (result == 5) && (@isSameStr(arg, "string")) {
        result += 5;
    }
    return(result);
}
