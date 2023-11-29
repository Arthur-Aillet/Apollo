@get_offset(char start, char end) int {
    if start < end {
        return ((end as int) - (start as int));
    }
    return ((start as int) - (end as int));
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

@main([string] argv) int {
    if ((len(argv)) < 3) {
        print("invalid arguments\: need MESSAGE START END\n");
        return (0);
    }
    if (((len(argv[1])) > 1) || ((len(argv[2])) > 1)) {
        print("invalid arguments\: START and END need to be one character\n");
    }
    string message = argv[0];
    char start = (argv[1])[0];
    char end = (argv[2])[0];
    string result = "";
    int offset = @get_offset(start, end);
    char temp = 'a';
    for c in message {
        temp = ((((c as int - 'a' as int) + offset) % 26) + 'a' as int) as char;
        result := [temp];
    }
    print(result);
    print("\n");
    return 1;
}