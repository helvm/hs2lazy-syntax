SRC = \
	hs/src/Lexer.hs \
	hs/src/Parser.hs \
	hs/src/Syntax.hs \
	hs/src/PPrint.hs \
	hs/src/Static.hs \
	hs/src/SCC.hs \
	hs/src/Type.hs \
	hs/src/PatComp.hs \
	hs/src/Compiler.hs \
	hs/src/Optimizer.hs \
	hs/src/Builtin.hs \
	hs/app/Main.hs

GHCFLAGS = -ihs/src
OPTFLAGS = -O
#PROFFLAGS = -prof -auto-all

all: hs2lazy examples

hs2lazy: $(SRC)
	ghc $(GHCFLAGS) $(OPTFLAGS) $(PROFFLAGS) -o $@ --make hs/app/Main.hs

Lexer.hs: Lexer.x
	alex Lexer.x

clean:
	rm -f hs/*/*.o hs/*/*.hi hs2lazy hs2lazy.exe

examples: hs2lazy
	make -C examples