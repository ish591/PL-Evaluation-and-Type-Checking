structure AST =
struct

type id = string
type const = string
datatype binop = Add | Sub | Mul | Eq | And | Or | Xor | Equals | Implies | lt | gt
datatype unop = Not | Negate
datatype decl = ValDecl of id * exp

and exp = NumExp of int
        | VarExp of id
        | BoolExp of const
        | IfthenExp of exp * exp * exp
    | BinExp of binop * exp * exp
    | LetExp of decl * exp
        | Unexp of unop * exp
        | AppExp of exp * exp
        | FnExp of id * typ * typ * exp
        | FunExp of id * id * typ * typ * exp

and typ =  INTtype | BOOLtype | Arrowtype of typ * typ

datatype program = SingleLineProgram of exp | MultiLineProgram of exp * program

datatype value = IntVal of int
           | BoolVal of bool
           | FnVal of id * id * exp * ((id * value) list) (*to store the function, we need its dummy variable and the expression associated with it. But we also 
            need the particular environment with it, as some previously defined functions may be used in the expression*)
end