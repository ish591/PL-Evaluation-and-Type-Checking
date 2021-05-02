structure typechecker = 
struct 


open AST

type typEnv = (id * typ) list

fun typEnvLookup (var:id, env:typEnv):typ = case List.find(fn(x, _) => x = var) env of
	SOME (x,v) => v
	| NONE => raise Fail("Variable" ^ var ^ "is without a type")

fun typEnvAdd (var : id, t : typ, env  : typEnv) :typEnv = (var,t)::env

fun typEnvRemove (var : id, t:typ, env : typEnv) = case env of [] => [] | (h1,t1)::tl => if (h1 = var andalso t1 = t) then tl else (h1,t1)::typEnvRemove(var,t,tl)

(*however, an identifier may be repeated later. In that case, take the innermost one, which occurs first in the list (since we add at head)*)
(*in case we encounter a function declaration, we just do an environment add amd print the function signature*)

fun typ1String(t:AST.typ):string = case t of
AST.INTtype => "INT"
| AST.BOOLtype => "BOOL"
| AST.Arrowtype(typ1,typ2) => "ARROW("^typ1String(typ1)^", "^typ1String(typ2)^")"

fun opError(t1:AST.typ, t2:AST.typ, b:AST.binop):string = case b of 
	Add => ("Type error: Expected types for ADD : (INT * INT), Found types : ("^typ1String(t1)^" * "^typ1String(t2)^")\n\n")
	| Sub => ("Type error: Expected types for MINUS : (INT * INT), Found types : ("^typ1String(t1)^" * "^typ1String(t2)^")\n\n")
	| Mul => ("Type error: Expected types for TIMES : (INT * INT), Found types : ("^typ1String(t1)^" * "^typ1String(t2)^")\n\n")
	| And =>("Type error: Expected types for AND : (BOOL * BOOL), Found types : ("^typ1String(t1)^" * "^typ1String(t2)^")\n\n")
	| Or => ("Type error: Expected types for OR : (BOOL * BOOL), Found types : ("^typ1String(t1)^" * "^typ1String(t2)^")\n\n")
	| Xor => ("Type error: Expected types for XOR : (BOOL * BOOL), Found types : ("^typ1String(t1)^" * "^typ1String(t2)^")\n\n")
	| Equals =>("Type error: Expected types for EQUALS : (BOOL * BOOL) or (INT * INT), Found types : ("^typ1String(t1)^" * "^typ1String(t2)^")\n\n")
	| Implies  =>("Type error: Expected types for IMPLIES : (BOOL * BOOL), Found types : ("^typ1String(t1)^" * "^typ1String(t2)^")\n\n")
	| lt =>("Type error: Expected types for LESSTHAN : (INT * INT), Found types : ("^typ1String(t1)^" * "^typ1String(t2)^")\n\n")
	| gt => ("Type error: Expected types for GREATERTHAN : (INT * INT), Found types : ("^typ1String(t1)^" * "^typ1String(t2)^")\n\n")
	| OTHERS => ("Invalid operator")
fun getType(e:exp, env:typEnv) : typ = 
	case e of 
		  NumExp _ => INTtype
		| BoolExp _ => BOOLtype
		| VarExp x => typEnvLookup(x,env)
		| AppExp (e1,e2) =>
		(case (getType(e1,env),getType(e2,env)) of
			(Arrowtype(t1,t2),t3) => 
			if (t1 = t3)
			then t2
			else raise Fail("Application argument type mismatch: Type of argument: "^(typ1String(t3))^", Expected type: "^(typ1String(t2))^"\n\n")
			| (_,_) => raise Fail("Function was expected, found :"^(typ1String(getType(e1,env)))^"\n\n"))
		| FnExp (x, t1, t2, e1) =>
			if (t2 = getType(e1,typEnvAdd(x,t1,env))) then 
			Arrowtype(t1,t2) (*also,remove x,t1 from the type environment*)
			else raise Fail("Return type of function does not match expression type: Type of expression "^(typ1String(getType(e1,typEnvAdd(x,t1,env))))^", Expected type: "^(typ1String(t2))^"\n\n")
			(*type of the environment must match the type of t2*) 
	| IfthenExp (e1,e2,e3) =>
		( let
			val t1 = getType(e1,env)
			val t2 = getType(e2,env)
			val t3 = getType(e3,env)
		   in
			if t1<> BOOLtype then
				raise Fail("Type error: Expected type in If condition : bool, Found: "^typ1String(t1)^"\n\n")
			else
				if t2<>t3 then
					raise Fail("Type error: Branches have different types, Type of Branch 1: "^typ1String(t2)^", Type of Branch 2: "^typ1String(t3)^"\n\n")
				else
					t2
		   end
		 )
		| FunExp(id1,id2,t1,t2,exp) =>
			(let 
				val expTyp = getType(exp,typEnvAdd(id1,Arrowtype(t1,t2), typEnvAdd(id2,t1,env)))
			in 
				if expTyp <>t2 then
					raise Fail("Mismatch in declared return type of function and the actual type, Declared type: "^typ1String(t2)^", Actual type : "^typ1String(expTyp)^"\n\n")
				else Arrowtype(t1,t2)
					(*why are we storing only the return type of the function here ??*)
			end
				)
		|  BinExp (binop, exp1, exp2) =>
			(*can only be applied to integers and booleans*)
				(if (getType(exp1,env) = getType(exp2,env) andalso (getType(exp1,env) = INTtype orelse getType(exp1,env) = BOOLtype)) then
					(case binop of 
						  Add =>
						 	if getType(exp1,env) = INTtype then
						 		INTtype
						 	else
						 		raise Fail(opError(getType(exp1,env),getType(exp2,env),binop))
						| Sub =>
							if getType(exp1,env) = INTtype then
						 		INTtype
						 	else
						 		raise Fail(opError(getType(exp1,env),getType(exp2,env),binop))

						| Mul =>
							if getType(exp1,env) = INTtype then
						 		INTtype
						 	else
						 		raise Fail(opError(getType(exp1,env),getType(exp2,env),binop))
						| And =>
							if getType(exp1,env) = BOOLtype then
						 		BOOLtype
						 	else
						 		raise Fail(opError(getType(exp1,env),getType(exp2,env),binop))
						| Or =>
							if getType(exp1,env) = BOOLtype then
						 		BOOLtype
						 	else
						 		raise Fail(opError(getType(exp1,env),getType(exp2,env),binop))
						| Xor =>
							if getType(exp1,env) = BOOLtype then
						 		BOOLtype
						 	else
						 		raise Fail(opError(getType(exp1,env),getType(exp2,env),binop))

						| Equals => BOOLtype
	
						| Implies  =>
							if getType(exp1,env) = BOOLtype then
						 		BOOLtype
						 	else
						 		raise Fail(opError(getType(exp1,env),getType(exp2,env),binop))
						| lt =>
							if getType(exp1,env) = INTtype then
						 		BOOLtype
						 	else
						 		raise Fail(opError(getType(exp1,env),getType(exp2,env),binop))
						| gt =>
							if getType(exp1,env) = INTtype then
						 		BOOLtype
						 	else
						 		raise Fail(opError(getType(exp1,env),getType(exp2,env),binop))
						 | OTHERS => raise Fail("Invalid operator")
					)
				else
					raise Fail(opError(getType(exp1,env),getType(exp2,env),binop))
					)

		| LetExp (ValDecl(x,e1),e2) =>
			let 
				val v1 = getType(e1, env)
			in
				getType(e2,typEnvAdd(x,v1,env))
			end


		| Unexp (unop,exp1) => 
			(case unop of Not =>
				if getType(exp1,env) = BOOLtype then
					BOOLtype
				else
					raise Fail("Type error: Expected type for Not : Bool, Actual type: "^typ1String(getType(exp1,env))^"\n\n")

			| Negate =>
				if getType(exp1,env) = INTtype then
					INTtype
				else
					raise Fail("Type error: Expected type for Negate : Int, Actual type: "^typ1String(getType(exp1,env))^"\n\n"))


end