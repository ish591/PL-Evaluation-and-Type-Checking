all:
	mlyacc COL226a3.yacc 
	mllex COL226a3.lex
	mlton a3.mlb
	
clean:
	rm -rf COL226a3.yacc.sig
	rm -rf COL226a3.yacc.desc
	rm -rf COL226a3.lex.sml
	rm -rf COL226a3.yacc.sml
	rm -rf a3

.SILENT: all clean