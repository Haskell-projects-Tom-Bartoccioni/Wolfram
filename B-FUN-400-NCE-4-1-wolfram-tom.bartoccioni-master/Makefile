##
## EPITECH PROJECT, 2021
## Wolfram
## File description:
## Makefile
##

SRC = `stack path --local-install-root`/bin/wolfram-exe

NAME =	wolfram	

all:	$(NAME)

$(NAME): $(SRC)
	cp $(SRC) ./$(NAME)

$(SRC): ./app/*.hs
	stack build

clean:
	stack clean

fclean:	clean
	rm -f $(NAME)

re:	fclean all