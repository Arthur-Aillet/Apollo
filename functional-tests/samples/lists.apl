@main ([string] args) int {
    int result = 0;
    [int] integers = [1, 2, 3, 4 ,5 ,6, 7, 8, 9, 0, 11, 12 ,13, 14, 15, 16, 17, 18, 19, 20];
    for i in "abcdefghijklmnopqrstuvwxyz" {
        result ++;
    }
    for i in integers {
        result += integers[(i % 20)];
    }
    return result;
}