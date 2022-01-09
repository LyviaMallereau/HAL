##
## EPITECH PROJECT, 2020
## B-FUN-300-MPL-3-1-funPoolDay2-corentin.petrau
## File description:
## Makefile
##


RM	=	rm -f
CC	=	ghc

SRC	=	app/Main.hs	\

TEST_SRC	=	test/Spec.hs	\


OBJ	=	$(SRC:.hs=.o)

NAME	=	hal

all:	$(NAME)

$(NAME):
	stack build 
	cp "$(shell stack path --local-install-root)/bin/hal-exe" ./
	mv hal-exe $(NAME)

clean:
	rm	-f	$(OBJ)
	rm	-f	app/*.hi
	rm	-f	app/*.o
	rm -f test/*.o
	rm -f test/*.hi

fclean:	clean
	rm	-f	$(NAME)
	rm -f unit_tests

tests_run:
	stack test --coverage

re:	fclean all