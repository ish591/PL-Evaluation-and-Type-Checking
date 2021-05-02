structure Tokens= Tokens
  
  type pos = int
  type columnNum =int
  type svalue = Tokens.svalue
  type ('a,'b) token = ('a,'b) Tokens.token  
  type lexresult = (svalue, pos ) token

  val pos = ref 1
  val tokenNum =ref 0
  val columnNum =ref 0
  val eof = fn () => (Tokens.EOF(!pos, !columnNum))
  val error = fn (e,l:int, col:int,tok) => TextIO.output(TextIO.stdOut,e^
  Int.toString(l)^":"^Int.toString(col)^":"^tok^"\n")

  fun revfold _ nil b = b
  | revfold f (hd::tl) b = revfold f tl (f(hd,b))
  
  val keywords =
  [
    ("IMPLIES", Tokens.IMPLIES),
    ("NOT", Tokens.NOT),
    ("AND", Tokens.AND),
    ("OR", Tokens.OR),
     ("XOR", Tokens.XOR),
    ("EQUALS", Tokens.EQUALS),
    ("if", Tokens.IF),
   ("then",  Tokens.THEN),
    ("else", Tokens.ELSE),
    ("fi", Tokens.FI),
    ("PLUS", Tokens.PLUS),
    ("MINUS", Tokens.MINUS),
    ("TIMES", Tokens.TIMES),
    ("NEGATE", Tokens.NEGATE),
    ("LESSTHAN", Tokens.LESSTHAN),
    ("GREATERTHAN", Tokens.GREATERTHAN),
   ("end",  Tokens.END),
   ("in",  Tokens.IN),
   ("let",  Tokens.LET),
   ("fn", Tokens.FN),
   ("fun", Tokens.FUN),
   ("int", Tokens.INT),
   ("bool", Tokens.BOOL)
   ]

val Conskeyword =
[
   ("TRUE", Tokens.CONST),
  ("FALSE", Tokens.CONST)
]
   
  fun findKeywords (str:string, pos1:pos, pos2:columnNum) =
  case List.find (fn (s, _) => s = str )  keywords of 
  SOME (_, tk) => tk(pos1,pos2)
  | NONE => case List.find(fn(s,_) => s = str) Conskeyword of SOME (_,tk) => tk (str,pos1,pos2) | NONE => Tokens.ID (str, pos1, pos2)
%%
%header (functor COL226a3LexFun(structure Tokens:COL226a3_TOKENS));

alpha=[A-Za-z];
ws = [\ \t];
digit = [0-9];
alphanum = [A-Za-z0-9];

%%
\n         => (columnNum := 0;pos := (!pos) + 1; lex());
\r\n       => (columnNum := 0;pos := (!pos) + 1; lex());
{ws}+      => (columnNum := (!columnNum)+1;lex());
{digit}+ => (Tokens.NUM(List.foldl (fn (a,r) => ord(a) - ord(#"0") + 10*r) 0 (explode yytext),!pos, !columnNum));
"~"{digit}+ =>
(Tokens.NUM(~(List.foldl (fn (a,r) => ord(a) - ord(#"0") + 10*r) 0 (explode (substring(yytext,1,size(yytext)-1)))),!pos, !columnNum));
";"        => (columnNum := (!columnNum)+1;
              Tokens.TERM(!pos,!columnNum));

"=>"        => (columnNum := (!columnNum)+1;
              tokenNum := (!tokenNum)+1;
              Tokens.DBLARROW(!pos,!columnNum));

"="        => (columnNum := (!columnNum)+1;
              Tokens.EQ(!pos,!columnNum));

"("        => (columnNum := (!columnNum)+1;
              tokenNum := (!tokenNum)+1;
              Tokens.LPAREN(!pos,!columnNum));

")"        => (columnNum := (!columnNum)+1;
              tokenNum := (!tokenNum)+1;
              Tokens.RPAREN(!pos,!columnNum));

":"        => (columnNum := (!columnNum)+1;
              tokenNum := (!tokenNum)+1;
              Tokens.COLON(!pos,!columnNum));
"->"        => (columnNum := (!columnNum)+1;
              tokenNum := (!tokenNum)+1;
              Tokens.ARROW(!pos,!columnNum));

{alpha}{alphanum}*   => (columnNum := (!columnNum)+size(yytext);
              findKeywords(yytext,!pos,!columnNum));

.          => (columnNum := (!columnNum)+1;
              error ("Unknown token:",!pos,!columnNum,yytext);lex());

