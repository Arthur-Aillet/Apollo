@main([string] args) int {
    int result = 0;
    int incr = 0;
    int incrtwo = 0;
    while (result < 100) {
        result ++;
    }
    while (incr < 5) {
        while (incrtwo < 10) {
            incrtwo ++;
            result ++;
        }
        incr ++;
        incrtwo = 0;
    }
    for i in ".................................................." {
        result ++;
    }
    return result;
}