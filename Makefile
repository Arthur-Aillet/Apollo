##
## EPITECH PROJECT, 2023
## Makefile
## File description:
## GLaDOS Makefile
##

NAME			=	glados

all:
	stack --local-bin-path . install

clean:
	stack clean
	rm -f .history

fclean: clean
	rm -f $(NAME)

re: fclean all

format:
	ormolu -m inplace $(shell find src test app -type f -name "*.hs")

format-check:
	ormolu -m 'check' $(shell find src test app -type f -name "*.hs")

unit-test:
	stack test --coverage
	stack hpc report --all --destdir test/coverage

func-test:
	./test/functional-tests.sh

test: unit-test func-test

.PHONY: all clean fclean re quick test unit-test func-test format format-check
