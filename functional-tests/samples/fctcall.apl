@addTwo(int a) int {
    return((a + 2));
}

@subone(int a) int {
    return(a - 1);
}

@addone(int a) int {
    return @subone((@addTwo(a)));
}

@main([string] argv) int {
    return @addone(9);
}