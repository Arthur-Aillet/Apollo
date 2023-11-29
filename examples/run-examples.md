# Acces au bash
## bash.apl

`./apollo run examples/bash.apl`
>ls:
>bash.apl
>cesar.apl
>fibonacci.apl
>run-examples.md
>strToint.apl
>
>cat:
>@main([string] argv) int {
>    print("ls\:\n");
>    print(($ls("examples")));
>    print("\ncat\:\n");
>    print(($cat("./examples/bash.apl")));
>    print("\n\npwd\:\n");
>    print(($pwd));
>    return 0;
>}
>
>pwd:
$PWD

Votre pwd devrais remplacer "\$PWD" dans le résultat
# manipulation de listes et de types
## cesar.apl
`./apollo run examples/cesar.apl -- abcdefghijklmnopqrstuvwxyz a c`
> cdefghijklmnopqrstuvwxyzab

Sentez vous libre de changer les parametres pour chiffrer votre propre message
# Aspects iteratifs
## fibonacci.apl
`./apollo run examples/fibonacci.apl -- 0 1 30`
>recursion simple finie: 832040
>recursion double finie: 832040
>boucle for finie: 832040
>boucle while finie: 832040

Les différent lapses de temps entre ces méssages reflètent a quel point ces méthodes sont optimisées
# Bases operatoires
## math.apl
`./apollo run examples/math.apl -- 1`
>25

Contient la pluspart des opérations applicables au type int
# sucre syntaxique et casts
## strToInt.apl
`./apollo run examples/strToint.apl -- 123`
>123

La chaîne de charactères donees en paramètre n'est pas simplement afichée, elle est transformVe en int puis en chaîne de charactères. c'est utilisé dans d'autres exemples pour afficher les résultats