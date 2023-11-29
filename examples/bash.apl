@main([string] argv) int {
    print("ls\:\n");
    print(($ls("examples")));
    print("\ncat\:\n");
    print(($cat("./examples/bash.apl")));
    print("\n\npwd\:\n");
    print(($pwd));
    return 0;
}