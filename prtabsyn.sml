structure PrintTAbsyn:
          sig
              val asString: TAbsyn.exp -> string
              val print: TextIO.outstream * TAbsyn.exp -> unit
          end =
struct

structure A = TAbsyn
structure S = Symbol
structure PT = PrintTypes

val i2s = Int.toString

fun printEncode s =
    let
        fun bs1 cs = #"\\" :: cs
        val bs2 = bs1 o bs1
        val bs3 = bs2 o bs1
        val bs4 = bs3 o bs1

        fun pe [] = []
          | pe (#"\\"::cs) = (bs2 o pe) cs
          | pe (#"\""::cs) = bs1 (#"\"" :: pe cs)
          | pe (#"\a"::cs) = bs1 (#"a" :: pe cs)
          | pe (#"\b"::cs) = bs1 (#"b" :: pe cs)
          | pe (#"\f"::cs) = bs1 (#"f" :: pe cs)
          | pe (#"\n"::cs) = bs1 (#"n" :: pe cs)
          | pe (#"\r"::cs) = bs1 (#"r" :: pe cs)
          | pe (#"\t"::cs) = bs1 (#"t" :: pe cs)
          | pe (#"\v"::cs) = bs1 (#"v" :: pe cs)
          | pe (c::cs) =
            let
                val ordc = ord (c)
                val charsc = Int.toString ordc
                val charsc' =
                    case size charsc
                     of 1 => "00" ^ charsc
                      | 2 => "0" ^ charsc
                      | _ => charsc
            in
                if 32 <= ordc andalso ordc < 128 then c :: pe cs
                else (bs1 (explode charsc')) @ (pe cs)
            end
    in
        (implode o pe o explode) s
    end

fun asString e0 =
    let
        fun indent 0 = ""
          | indent i = indent (i-1) ^ (if i mod 2 = 0 then "| " else "  ")

        fun opname A.PlusOp = "PlusOp"
          | opname A.MinusOp = "MinusOp"
          | opname A.TimesOp = "TimesOp"
          | opname A.DivideOp = "DivideOp"
          | opname A.EqOp = "EqOp"
          | opname A.NeqOp = "NeqOp"
          | opname A.LtOp = "LtOp"
          | opname A.LeOp = "LeOp"
          | opname A.GtOp = "GtOp"
          | opname A.GeOp = "GeOp"
          | opname A.ExponentOp = "ExponentOp"

        fun dolist d f [a] = "\n" ^ f (a, d+1)
          | dolist d f (a::r) = "\n" ^ f (a, d+1) ^
                                "," ^ dolist d f r
          | dolist d f nil = ""

        fun var ({ var = A.SimpleVar s
                 , ty = ty}, d) =
            indent d ^
            "SimpleVar(" ^
	    S.name s ^
            ", " ^ (PT.asString ty) ^ ")"
          | var ({ var = A.FieldVar (v, s)
                 , ty = ty}, d) =
            indent d ^
            "FieldVar(\n" ^
	    var (v, d+1) ^
            ",\n" ^
	    indent (d+1) ^
            S.name s ^
            ", " ^ (PT.asString ty) ^ ")"
          | var ({ var = A.SubscriptVar (v, e)
                 , ty = ty }, d) =
            indent d ^
            "SubscriptVar(\n" ^
	    var (v, d+1) ^
            ",\n" ^
	    exp (e, d+1) ^ ")"
        and exp ({exp = e, ty = t}, d) =
            indent d ^  "(" ^ PT.asString t ^ ", " ^ (exp_desc (e, d+1)) ^ ")"
        and exp_desc (A.VarExp v, d) =
            "VarExp(\n" ^
            var (v, d+1) ^
            ")"
          | exp_desc (A.NilExp, d) =
            "NilExp"
          | exp_desc (A.IntExp i, d) =
            "IntExp(" ^
            i2s i ^
	    ")"
          | exp_desc (A.StringExp s, d) =
            "StringExp(\"" ^
	    printEncode s ^
            "\")"
          | exp_desc (A.CallExp {func, args}, d) =
            "CallExp(" ^
            S.name func ^
	    ",[" ^
            dolist d exp args ^
            "])"
          | exp_desc (A.OpExp {left, oper, right}, d) =
            "OpExp(" ^
            opname oper ^
            ",\n" ^
	    exp (left, d+1) ^
            ",\n" ^
            exp (right, d+1) ^
            ")"
          | exp_desc (A.RecordExp {fields}, d) =
	    let fun f ((name, e), d) =
		    indent d ^
                    "(" ^
                    S.name name ^
		    ",\n" ^
                    exp (e, d+1) ^
		    ")"
	    in "RecordExp([" ^
               dolist d f fields ^
               "])"
	    end
          | exp_desc (A.SeqExp l, d) =
            "SeqExp[" ^
            dolist d exp l ^
	    "]"
          | exp_desc (A.AssignExp {var = v, exp = e}, d) =
            "AssignExp(\n" ^
            var (v, d+1) ^
            ",\n" ^
	    exp (e, d+1) ^
            ")"
          | exp_desc (A.IfExp {test, thn, els}, d) =
            "IfExp(\n" ^
            exp (test, d+1) ^
            ",\n" ^
	    exp (thn, d+1) ^
	    (case els of NONE => ""
		       | SOME e => ",\n" ^ exp (e, d+1)) ^
	    ")"
          | exp_desc (A.WhileExp {test, body}, d) =
            "WhileExp(\n" ^
            exp (test, d+1) ^
            ",\n" ^
	    exp (body, d+1) ^
            ")"
          | exp_desc (A.ForExp {var = v, escape = b, lo, hi, body}, d) =
            "ForExp(" ^
	    S.name v ^
            "," ^
            Bool.toString (!b) ^
            ",\n" ^
	    exp (lo, d+1) ^
            ",\n" ^
            exp (hi, d+1) ^
            ",\n" ^
	    exp (body, d+1) ^
            ")"
          | exp_desc (A.BreakExp, d) =
            "BreakExp"
          | exp_desc (A.LetExp {decls, body}, d) =
            "LetExp([" ^
	    dolist d dec decls ^
            "],\n" ^
            exp (body, d+1) ^
            ")"
          | exp_desc (A.ArrayExp {size, init}, d) =
            "ArrayExp(\n" ^
	    exp (size, d+1) ^
            ",\n" ^
            exp (init, d+1) ^
            ")"
          | exp_desc (A.ErrorExp,d) =
            "ErrorExp"

        and dec (A.FunctionDec l, d) =
	    let fun field ({ name: S.symbol
                           , escape: bool ref
                           , ty: Types.ty}, d) =
		    indent d ^
                    "(" ^
                    S.name name ^
		    "," ^
                    Bool.toString (!escape) ^
		    "," ^
                    (PT.asString ty) ^
                    ")"
		fun f ({ name: S.symbol
                       , params: A.fielddata list
                       , resultTy: Types.ty
                       , body: A.exp}, d) =
		    indent d ^
                    "(" ^
                    S.name name ^
                    ",[" ^
		    dolist d field params ^
                    "],\n" ^
                    indent (d+1) ^
                    PT.asString resultTy ^
                    ",\n" ^
                    exp (body, d+1) ^
                    ")"
	    in indent d ^
               "FunctionDec[" ^
               dolist d f l ^
               "]"
	    end

          | dec (A.VarDec {name, escape, ty, init}, d) =
	    indent d ^
            "VarDec(" ^
            S.name name ^
            "," ^
	    Bool.toString (!escape) ^
            "," ^ (PT.asString ty) ^
            ",\n" ^
            exp (init, d+1) ^
            ")"
          | dec (A.TypeDec l, d) =
	    let fun tdec ({name, ty = t}, d) =
                    indent d ^
                    "(" ^
		    S.name name ^
                    ",\n" ^
		    indent (d + 1) ^ PT.asString t ^ ")"
	    in indent d ^
               "TypeDec[" ^
               dolist d tdec l ^
               "]"
            end

        and ty (typ, d) = (indent d) ^ (PT.asString typ)

    in  exp (e0, 0) ^
        "\n"
    end

fun print (outstream, e0) =
    ( TextIO.output (outstream, asString e0)
    ; TextIO.flushOut outstream)

end (* PrintAbsyn *)
