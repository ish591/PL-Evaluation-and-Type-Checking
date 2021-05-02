structure EVALUATOR  =
struct
open AST

val brokenTypes = Fail "Error in evaluation!"

type environment = (id * value) list

fun envLookup (var:id, env:environment):value = case List.find(fn(x, _) => x = var) env of
  SOME (x,v) => v
  | NONE => raise Fail("Variable " ^ var ^ " is without a type")

fun checkInEnv (var:id, env:environment):bool = case List.find(fn(x, _) => x = var) env of
  SOME (x,v) => true
  | NONE => false

fun envAdd (var : id, t : value, env  : environment) :environment = (var,t)::env

fun evalExp(e:exp, env:environment):value =
    case e of
        NumExp i           => IntVal i
      | BoolExp b        => if b = "TRUE" then BoolVal(true) else BoolVal(false)
      | VarExp x            => envLookup (x, env)           
      | BinExp (b, e1, e2)  => evalBinExp(b, e1, e2, env)
      | LetExp(ValDecl(x, e1), e2)  =>
    let
      val v1 = evalExp (e1, env)
  in
      evalExp(e2, envAdd (x, v1, env))
        end 
      | IfthenExp (e1,e2,e3) =>   if evalExp(e1,env) = BoolVal(true) then evalExp(e2,env) else evalExp(e3,env)
      | Unexp (unop, e1) => if unop = Not then 
                              (case evalExp(e1,env) of BoolVal(true) => BoolVal(false) | BoolVal(false) => BoolVal(true) | OTHERS => raise Fail("Invalid"))
                            else
                              (case evalExp(e1,env) of IntVal(i) => IntVal(~i) | OTHERS => raise Fail("Invalid"))
      | AppExp (exp1, exp2) => (case evalExp(exp1,env) of FnVal(id1,id2,exp3,envOld) => (case checkInEnv(id1,envOld) of true => evalExp(exp3,envAdd(id2,evalExp(exp2,env),envOld)) | false => evalExp(exp3,envAdd(id1,FnVal(id1,id2,exp3,envOld),envAdd(id2,evalExp(exp2,env),envOld)))) | OTHERS => raise Fail("Invalid"))
        (*value of id used in evaluation will be evaluated value of exp2*)
      | FnExp (id,typ1,typ2,exp) => FnVal("",id,exp,env)
      | FunExp (id1, id2, typ1, typ2, exp) => FnVal(id1,id2,exp,env)
      (*anonymous functions have their usual function types*)


and
evalBinExp(b:binop, e1:exp, e2:exp, env:environment):value =
case (b, evalExp(e1, env), evalExp(e2, env))  of
    (Add, IntVal i1, IntVal i2) => IntVal (i1+i2)
  |   (Sub, IntVal i1, IntVal i2) => IntVal (i1-i2)
  |   (Mul, IntVal i1, IntVal i2) => IntVal (i1*i2)
  |   (Equals, BoolVal i1, BoolVal i2) => BoolVal (i1 = i2)
  |   (Equals, IntVal i1, IntVal i2) => BoolVal (i1 = i2)
  |   (And, BoolVal i1, BoolVal i2) => BoolVal (i1 andalso i2)
  |   (Or, BoolVal i1, BoolVal i2) => BoolVal (i1 orelse i2)
  |   (Xor, BoolVal i1, BoolVal i2) => BoolVal (i1 <> i2)
  |   (Implies, BoolVal i1, BoolVal i2) => (if i1 = true andalso i2 = false then BoolVal(false) else BoolVal(true))
  |   (lt, IntVal i1, IntVal i2) => BoolVal (i1 < i2)
  |   (gt, IntVal i1, IntVal i2) => BoolVal (i1 > i2)
  |   _  => raise brokenTypes    


end
(*however, an identifier may be repeated later. In that case, take the innermost one, which occurs first in the list (since we add at head)*)
(*in case we encounter a function declaration, we just do an environment add amd print the function signature*)
