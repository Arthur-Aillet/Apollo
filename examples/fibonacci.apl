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

@fibonacci_For(int first, int second, int n) int {
    int l = first;
    int r = second;
    [int] iterable = @create_iterable(n);
    int next = 0;
    for j in iterable {
        next = l + r;
        l = r;
        r = next;
    }
    return l;
}

@create_iterable(int n) [int] {
    int iterator = 0;
    [int] result = [];
    while (iterator < n) {
        iterator ++;
        result := [iterator];
    }
    return result;
}

@fibonacci_While(int first, int second, int n) int {
    int iterator = 0;
    int l = first;
    int r = second;
    int next = 0;
    while (iterator < n) {
        iterator ++;
        next = l + r;
        l = r;
        r = next;
    }
    return l;
}

@fibonacci_Double(int first, int second, int n) int {
    if (n == first) || (n == second)  {
        return (n);
    }
    return ((@fibonacci_Double(first, second, (n - 1))) + (@fibonacci_Double(first, second, (n - 2))));
}

@fibonacci_Simple(int first, int second, int n) int {
    if (n == 0) {
        return (first);
    }
    return (@fibonacci_Simple(second, (first + second), (n - 1)));
}

@main ([string] argv) int {
    if ((len(argv)) < 3) {
        print("invalid arguments\: need first second iterations\n");
        return (0);
    }
    int first = @strToInt((argv[0]));
    int second = @strToInt((argv[1]));
    int n = @strToInt((argv[2]));
    int result_simple = @fibonacci_Simple(first, second, n);
    print ("recursion simple finie\: ");
    print((@intToStr(result_simple)));
    print("\n");
    int result_double = @fibonacci_Double(first, second, n);
    print ("recursion double finie\: ");
    print((@intToStr(result_double)));
    print("\n");
    int result_for = @fibonacci_For(first, second, n);
    print ("boucle for finie\: ");
    print((@intToStr(result_for)));
    print("\n");
    int result_while = @fibonacci_While(first, second, n);
    print ("boucle while finie\: ");
    print((@intToStr(result_while)));
    print("\n");
    return result_for;
}