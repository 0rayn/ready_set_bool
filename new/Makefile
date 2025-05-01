NAME = readySetBool
CMP = ghc
MAIN = main.hs
SRC = adder.hs
FLAGS = -no-keep-hi-files -no-keep-o-files

all: $(NAME)

$(NAME): $(SRC) $(MAIN)
	$(CMP) $(FLAGS) $(MAIN) --make $(SRC) -o $(NAME)

run: $(NAME)
	@echo "------------------------------------------------"
	@echo "---               RUNNING                    ---"
	@echo "------------------------------------------------"
	./$(NAME)

clean:
	rm $(NAME)

.Phony: run all