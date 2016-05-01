(* AU Compilation 2015.
 *
 * Do not change this file, unless you suspect an error.
 * Use the course web forum to discuss that.
 *
 *)




structure TAbsyn =
struct

structure S = Symbol

datatype var_desc = SimpleVar of S.symbol
                  | FieldVar of var * S.symbol
                  | SubscriptVar of var * exp

and exp_desc = VarExp of var
             | NilExp
             | IntExp of int
             | StringExp of string
             | CallExp of calldata
             | OpExp of opdata
             | RecordExp of rcxdata
             | SeqExp of exp list
             | AssignExp of asxdata
             | IfExp of ifdata
             | WhileExp of whiledata
             | ForExp of fordata
             | BreakExp
             | LetExp of letdata
             | ArrayExp of arxdata
             | ErrorExp

and decl = FunctionDec of fundecldata list
         | VarDec of vardecldata
         | TypeDec of tydecldata list

and oper     = EqOp | NeqOp
             | LtOp | LeOp | GtOp | GeOp
             | PlusOp | MinusOp | TimesOp | DivideOp | ExponentOp

withtype var = { var : var_desc
               , ty : Types.ty }

and exp = { exp : exp_desc
          , ty : Types.ty }

and fielddata   = { name: S.symbol
                  , escape: bool ref
                  , ty: Types.ty}

and      fundecldata = { name: S.symbol
                       , params: fielddata list
                       , resultTy: Types.ty
                       , body: exp}
and      vardecldata = { name: S.symbol
                       , escape: bool ref
                       , ty: Types.ty
                       , init: exp}
and      tydecldata  = { name: S.symbol
                       , ty: Types.ty}
and      fordata     = { var : S.symbol
                       , escape: bool ref
                       , lo: exp
                       , hi: exp
                       , body: exp}
and      whiledata   = { test: exp
                       , body: exp}
and      letdata     = { decls: decl list
                       , body: exp}
and      ifdata      = { test: exp
                       , thn: exp
                       , els: exp option}
and      calldata    = { func: S.symbol
                       , args: exp list}
and      opdata      = { left: exp
                       , oper: oper
                       , right: exp}
and      arxdata     = { size: exp
                       , init: exp}
and      asxdata     = { var: var
                       , exp: exp}
and      rcxdata     = { fields: (S.symbol * exp) list}

end (* TAbsyn *)
