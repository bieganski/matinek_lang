all:
	happy -gca ParGrammar.y
	alex -g LexGrammar.x
	ghc --make TestGrammar.hs -o TestGrammar
	ghc Interpreter.hs -o interpreter

clean:
	-rm -f *.log *.aux *.hi *.o *.dvi

distclean: clean
	-rm -f DocGrammar.* LexGrammar.* ParGrammar.* LayoutGrammar.* SkelGrammar.* PrintGrammar.* TestGrammar.* AbsGrammar.* TestGrammar ErrM.* SharedString.* ComposOp.* Grammar.dtd XMLGrammar.* Makefile
	
