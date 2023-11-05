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
    arg := ("abc" : ['c', 'b', 'a']);
    if @isSameStr(arg, "testabccba") {
        return 1;
    }
    return 0;
}