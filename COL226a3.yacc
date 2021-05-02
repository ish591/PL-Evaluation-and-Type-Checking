(* User  declarations *)


%%
(* required declarations *)
%name COL226a3

%term
 ID of string | NUM of int | CONST of string | TERM | NOT | AND | OR | XOR | EQUALS | IMPLIES | IF | THEN | ELSE
 | LPAREN | RPAREN | EOF | PLUS | TIMES | MINUS | NEGATE | LESSTHAN | GREATERTHAN | EQ | FI | LET | IN | END | ARROW | DBLARROW | COLON | FN | FUN | INT | BOOL 

%nonterm START of AST.program | program of AST.program | statement of AST.exp | formula of AST.exp | DECL of AST.decl | TYPE of AST.typ

%pos int

(*optional declarations *)
%eop EOF
%noshift EOF


(* %header  *)
%right IF THEN ELSE FI
%right IMPLIES
%left AND OR XOR EQUALS EQ
%right NOT
%nonassoc ID CONST TERM LPAREN RPAREN LET IN END DBLARROW
%left LESSTHAN GREATERTHAN
%left MINUS PLUS
%left TIMES
%right NEGATE ARROW 
%start START

%verbose

%%
  START : program (program)

  program: statement TERM program (AST.MultiLineProgram(statement,program)) | statement (AST.SingleLineProgram(statement))

  statement : formula (formula) 

  DECL : ID EQ formula (AST.ValDecl(ID,formula))

  formula :  LPAREN formula RPAREN (formula)  
           | IF formula THEN formula ELSE formula FI (AST.IfthenExp(formula1, formula2, formula3)) 
           | LET DECL IN formula END (AST.LetExp(DECL,formula)) 
           | LPAREN formula formula RPAREN (AST.AppExp(formula1,formula2))
          |  CONST   (AST.BoolExp(CONST))
          | NOT formula (AST.Unexp(AST.Not,formula))
          | formula AND formula (AST.BinExp(AST.And, formula1, formula2))
          | formula OR formula (AST.BinExp(AST.Or, formula1, formula2))
          | formula XOR formula (AST.BinExp(AST.Xor, formula1, formula2))
          | formula EQUALS formula (AST.BinExp(AST.Equals, formula1, formula2))
          | formula IMPLIES formula (AST.BinExp(AST.Implies, formula1, formula2))
          | ID (AST.VarExp(ID))
          | NUM (AST.NumExp(NUM))
          | formula PLUS formula (AST.BinExp(AST.Add, formula1, formula2))
          | formula MINUS formula (AST.BinExp(AST.Sub, formula1, formula2))
          | formula TIMES formula (AST.BinExp(AST.Mul, formula1,formula2))
          | NEGATE formula (AST.Unexp(AST.Negate,formula))
          | formula LESSTHAN formula (AST.BinExp(AST.lt, formula1, formula2))
          | formula GREATERTHAN formula (AST.BinExp (AST.gt, formula1, formula2))
          | FN LPAREN ID COLON TYPE RPAREN COLON TYPE DBLARROW formula (AST.FnExp(ID,TYPE1,TYPE2, formula))
          | FUN ID LPAREN ID COLON TYPE RPAREN COLON TYPE DBLARROW formula (AST.FunExp(ID1,ID2,TYPE1,TYPE2,formula))

TYPE: LPAREN TYPE RPAREN (TYPE) | INT (AST.INTtype) | BOOL (AST.BOOLtype)| TYPE ARROW TYPE (AST.Arrowtype(TYPE1,TYPE2))


