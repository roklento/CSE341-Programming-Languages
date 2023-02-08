gpp_lexer.out:	gpp_lexer.c
		yacc -d gpp_interpreter.y
		lex -o gpp_interpreter.c gpp_lexer.l
		cc -g -w -lm -std=gnu99 gpp_interpreter.c y.tab.c -ll
