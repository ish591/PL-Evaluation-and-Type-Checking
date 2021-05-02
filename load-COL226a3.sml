structure COL226a3LrVals = COL226a3LrValsFun(structure Token = LrParser.Token)
structure COL226a3Lex = COL226a3LexFun(structure Tokens = COL226a3LrVals.Tokens);
structure COL226a3Parser =
	  Join(structure LrParser = LrParser
     	       structure ParserData = COL226a3LrVals.ParserData
     	       structure Lex = COL226a3Lex)

fun space_seperator(s)=
(*gives the last word from s*)
    let
       fun strr (nil, curString,encSpace)= curString |
        strr(hd::tl, curString, encSpace)=
            if hd = #" " then
                strr(tl, curString, true)
            else
                if encSpace then
                    strr(tl,str(hd),false)
                else
                    strr(tl,curString^str(hd),false)
    in
    strr(explode(s),"",false)
end

fun productionRule(s)=
    if s = "ID" then " formula => ID"
    else if s = "CONST" then " formula => CONST"
    else if s = "LPAREN" then " formula => LPAREN formula RPAREN"
    else if s = "RPAREN" then " formula => LPAREN formula RPAREN"
    else if s= "IF" then " formula => IF formula THEN formula ELSE formula"
    else if s= "THEN" then " formula => IF formula THEN formula ELSE formula"
    else if s= "ELSE" then " formula => IF formula THEN formula ELSE formula"
    else if s="NOT" then " formula => NOT formula"
    else if s="AND" then " formula => formula AND formula"
    else if s="OR" then " formula => formula OR formula"
    else if s="XOR" then " formula => formula XOR formula"
    else if s="EQUALS" then " formula => formula EQUALS formula"
    else if s="IMPLIES" then " formula => formula IMPLIES formula"
    else if s="TERM" then " statement => formula TERM"
    else s

fun invoke lexstream =
    	     	let fun print_error (s,pos:int,columnNum:int) =
		    	TextIO.output(TextIO.stdOut, "Syntax Error:" ^ (Int.toString pos) ^ ":" ^ (Int.toString columnNum)^ ":" ^ productionRule(space_seperator(s)) ^ "\n")
		in
		    COL226a3Parser.parse(0,lexstream,print_error,())
		end

fun stringToLexer str =
    let val done = ref false
    	val lexer=  COL226a3Parser.makeLexer (fn _ => if (!done) then "" else (done:=true;str))
    in
	lexer
    end	
		
fun parse (lexer) =
    let val dummyEOF = COL226a3LrVals.Tokens.EOF(0,0)
    	val (result, lexer) = invoke lexer
	val (nextToken, lexer) = COL226a3Parser.Stream.get lexer
    in
        if COL226a3Parser.sameToken(nextToken, dummyEOF) then result
 	else (TextIO.output(TextIO.stdOut, "Warning: Unconsumed input \n"); result)
    end

fun declString (t: AST.decl ):string = case t of AST.ValDecl(ID1,formula1) => "ValDecl("^ID1^", "^expString(formula1)^")"

and

expString(t:AST.exp):string = case t of
 AST.NumExp(num1) => "NumExp("^Int.toString(num1)^")"
  | AST.VarExp(id1) => "VarExp("^id1^")"
  | AST.AppExp(exp1,exp2) => "AppExp("^expString(exp1)^", "^expString(exp2)^")"
  | AST.FnExp(id,typ1,typ2,exp) => "Fn("^id^", "^typString(typ1)^", "^typString(typ2)^", "^expString(exp)
 | AST.BoolExp(const) => "BoolExp("^const^")"
| AST.IfthenExp (formula1,formula2,formula3) => "IfthenExp("^expString(formula1)^", "^expString(formula2)^", "^expString(formula3)^")"
 | AST.BinExp(binop1,formula1,formula2) => (case binop1 of 
 AST.Add => "BinExp(PLUS, "^expString(formula1)^", "^expString(formula2)^")"
 |AST.Sub => "BinExp(SUB, "^expString(formula1)^", "^expString(formula2)^")"
 |AST.Mul => "BinExp(TIMES, "^expString(formula1)^", "^expString(formula2)^")"
 |AST.Eq  => "BinExp('=', "^expString(formula1)^", "^expString(formula2)^")"
 |AST.And => "BinExp(AND, "^expString(formula1)^", "^expString(formula2)^")"
 |AST.Or  => "BinExp(OR, "^expString(formula1)^", "^expString(formula2)^")"
 |AST.Xor => "BinExp(XOR, "^expString(formula1)^", "^expString(formula2)^")"
 |AST.Equals => "BinExp(EQUALS, "^expString(formula1)^", "^expString(formula2)^")"
 |AST.Implies => "BinExp(IMPLIES, "^expString(formula1)^", "^expString(formula2)^")"
 |AST.lt => "BinExp(LESSTHAN, "^expString(formula1)^", "^expString(formula2)^")"
 |AST.gt => "BinExp(GREATERTHAN, "^expString(formula1)^", "^expString(formula2)^")")
 | AST.LetExp(decl1,formula) => "LetExp("^declString(decl1)^","^expString(formula)^")"
 | AST.Unexp (unop1, formula) => (case unop1 of AST.Not => "Unexp(NOT,"^expString(formula)^")" | AST.Negate =>  "Unexp(NEGATE,"^expString(formula)^")")
 | AST.FunExp (id1,id2,typ1,typ2,exp) =>  "Fun("^id1^", "^id2^", "^typString(typ1)^", "^typString(typ2)^", "^expString(exp)^")"

 and

programString (t:AST.program,l: int):string =  case t of 
AST.SingleLineProgram(exp1) =>  "Exp/decl "^Int.toString(l)^" : "^expString(exp1)^"\n" 
| AST.MultiLineProgram(exp1,program1) =>"Exp/decl "^Int.toString(l)^" : "^expString(exp1)^"\n"^programString(program1,l+1) 

and

typString(t:AST.typ):string = case t of
AST.INTtype => "INT"
| AST.BOOLtype => "BOOL"
| AST.Arrowtype(typ1,typ2) => "ARROW("^typString(typ1)^", "^typString(typ2)^")"

and 

evalString(t:AST.value):string = case t of 
AST.IntVal(i1) => "INT : "^Int.toString(i1)
| AST.BoolVal(i1) => (case i1 of true => "BOOL : True" | false => "BOOL : False" )
| AST.FnVal(id,id2,exp,env) => "This is a function"

fun typecheck(t:AST.program, env:typechecker.typEnv, l:int):string = case t of 
AST.SingleLineProgram(exp1) =>  "Exp/decl "^Int.toString(l)^" : "^typString(typechecker.getType(exp1,env)) ^ "\n"    
(*in case of function declaration, we add to the environment if it is a multi-line program*)
| AST.MultiLineProgram(exp1, program1) => (case exp1 of 
    AST.FunExp (id1,id2,typ1,typ2,exp) => "Exp/decl "^Int.toString(l)^" : "^typString(typechecker.getType(exp1,env)) ^ "\n" ^ typecheck(program1, typechecker.typEnvAdd(id1,typechecker.getType(exp1,env),env),l+1)
| OTHERS => "Exp/decl "^Int.toString(l)^" : "^typString(typechecker.getType(exp1,env)) ^ "\n" ^ typecheck(program1,env,l+1))

fun evaluate(t:AST.program, env: EVALUATOR.environment, l:int):string = case t of 
AST.SingleLineProgram(exp1) =>  "Exp/decl "^Int.toString(l)^" : "^evalString(EVALUATOR.evalExp(exp1,env)) ^ "\n"    
(*in case of function declaration, we add to the environment if it is a multi-line program*)
| AST.MultiLineProgram(exp1, program1) => (case exp1 of 
    AST.FunExp (id1,id2,typ1,typ2,exp) => "Exp/decl "^Int.toString(l)^" : "^evalString(EVALUATOR.evalExp(exp1,env)) ^ "\n" ^ evaluate(program1, EVALUATOR.envAdd(id1,EVALUATOR.evalExp(exp1,env),env),l+1)
| OTHERS => "Exp/decl "^Int.toString(l)^" : "^evalString(EVALUATOR.evalExp(exp1,env)) ^ "\n" ^ evaluate(program1,env,l+1))

val parseString = parse o stringToLexer;    (*returns a AST*)
val args = CommandLine.arguments();
fun giveFile (nil:string list) = "" | giveFile (hd::tl :string list)= hd;
val file =giveFile args;
print("\nThe abstract syntax tree for the program is printed below : \n");
print("\n"^programString(parseString(TextIO.inputAll(TextIO.openIn (file))),1)^"\n");
print("\n"^"The types of various expressions are listed below: \n \n");
print(typecheck(parseString(TextIO.inputAll(TextIO.openIn (file))), [],1)); (*to perform type checking on the AST, print the types of all the expressions*)
print("\n"^"The result of evaluation of various expressions is listed below: \n \n");
print(evaluate(parseString(TextIO.inputAll(TextIO.openIn (file))),[],1) ^ "\n");





