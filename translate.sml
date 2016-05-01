structure Translate: TRANSLATE =
struct

structure F = X86Frame
structure TAbs = TAbsyn
structure T = Tree
structure PT = PrintTree(F)

exception Bug

val err = ErrorMsg.error

datatype level =
         Top
       | Level of {frame: F.frame, parent: level} * unit ref

type access = level * F.access

datatype exp = Ex of Tree.exp
             | Nx of Tree.stm
             | Cx of Temp.label * Temp.label -> Tree.stm

type breakpoint = Tree.label

type frag = F.frag

val bogus = Ex (T.CONST 0)

local
    val frags: (frag list) ref = ref []
in
    fun addFrag frag = (frags := frag::(!frags))
    fun getFrags () = !frags
end

val outermost = Top

exception Bug of string

fun newLevel {parent=l, name=n, formals=f} =
    let
        (* 'true': static link, which always escapes *)
        val frame = F.newFrame {name = n, formals = true::f}
    in
        Level ({frame = frame, parent = l}, ref ())
    end

fun allocLocal Top _ =
    raise Bug "attempt to allocate local variable in top scope"
  | allocLocal (l as Level ({frame, parent}, _)) b =
    (l, F.allocLocal frame b)

fun accessOfFormal l nr escaping =
    (l, F.accessOfFormal nr escaping)

fun formals Top =
    raise Bug "attempt to get formals of top scope"
  | formals (l as Level ({frame,parent}, _)) =
    let
        val fFormals = F.formals frame
        fun facc2acc level facc = (level,facc)
    in
        map (facc2acc l) fFormals
    end

fun seq [] = T.EXP (T.CONST 0)
  | seq [s] = s
  | seq (h::t) = T.SEQ (h, seq t)

fun unEx (Ex e) = e
  | unEx (Cx genstm) =
    let
        val r = Temp.newtemp ()
        val t = Temp.newLabel "unEx_t"
        val f = Temp.newLabel "unEx_f"
    in
        T.ESEQ ( seq [ T.MOVE (T.TEMP r, T.CONST 1)
                     , genstm (t, f)
                     , T.LABEL f
                     , T.MOVE (T.TEMP r, T.CONST 0)
                     , T.LABEL t]
               , T.TEMP r)
    end
  | unEx (Nx s) = T.ESEQ (s, T.CONST 0)

fun unNx (Ex e) = T.EXP e
  | unNx (Cx genstm) =
    let
	val t = Temp.newLabel "true"
    in
	    T.SEQ(genstm(t,t), T.LABEL t)
    end
  | unNx (Nx s) = s

fun unCx (Ex (T.CONST 0)) = (fn (t,f) => T.JUMP(T.NAME f, [f]))
  | unCx (Ex (T.CONST _)) = (fn (t,f) => T.JUMP(T.NAME t, [t]))
  | unCx (Ex e) = (fn (t,f) => T.CJUMP(T.EQ, e, T.CONST 0, f, t))
  | unCx (Cx genstm) = genstm
  | unCx (Nx _) = raise Bug "Should never occur"

val empty = Ex (T.CONST 0)

val newBreakPoint = Temp.newLabel

fun levelEq (Level (_, u1), Level (_, u2)) = (u1 = u2)
  | levelEq _ = false

fun followStaticLink toLevel (fromLevel as Level ({frame, parent}, _)) =
    let
      val sloffset =  hd (F.formals(frame))
    in
      if levelEq (toLevel, fromLevel)
      then T.TEMP F.FP
      else T.MEM(T.BINOP(T.PLUS,T.CONST(F.staticLinkOffset frame), followStaticLink toLevel parent))
    end
  | followStaticLink _ Top =
    T.TEMP F.FP (* delivered to built-in functions like chr,ord,.. *)

fun simpleVar ((toLevel, fFrame), fromLevel) =
  let
      val frp = followStaticLink  toLevel fromLevel
      val sVar = F.exp fFrame frp
  in
      Ex (sVar)
  end

fun fieldVar (var, offset) =

let
    val var' = unEx var
    val nilL = Temp.newLabel "field_nil"
    val nonNilL = Temp.newLabel "field_nonNil"
    val addressT = Temp.newtemp ()
    val varT = Temp.newtemp ()
in
  Ex (T.ESEQ(seq [T.MOVE(T.TEMP varT, var')
     , T.CJUMP(T.EQ, T.TEMP varT, T.CONST(0), nilL, nonNilL)
		 , T.LABEL nilL
		 , T.EXP (F.externalCall("recFieldError", [])) (* No JUMP because exit(1) is called *)
		 , T.LABEL nonNilL
		 , T.MOVE(T.TEMP addressT, T.BINOP(T.PLUS, T.TEMP varT,
						   (T.BINOP(T.MUL,T.CONST(offset), T.CONST(F.wordSize)))))]
	   ,T.MEM(T.TEMP addressT)))
end

fun assign2IR (var, exp) =
    let
        val var = unEx var
        val exp = unEx exp
    in
        Nx (T.MOVE (var, exp))
    end

fun break2IR break =
    Nx (T.JUMP (T.NAME break, [break]))

fun int2IR i = Ex (T.CONST i) (* Return a constant of that value *)

fun nil2IR () = Ex (T.CONST 0)

fun ifThen2IR (test, thenExp) =
    let
        val test' = unCx test
        val labelThen = Temp.newLabel "if_then"
        val labelEnd = Temp.newLabel "if_end"
    in
        case (test', thenExp)
         of (_, Cx func) =>
            Cx (fn (t, f) =>
                   seq [ test' (labelThen, labelEnd)
                       , T.LABEL labelThen
                       , func (t, f)
                       , T.LABEL labelEnd])
          | (_, Nx _) =>
            Nx (seq [test' (labelThen, labelEnd)
                , T.LABEL labelThen
                , unNx(thenExp)
                , T.LABEL labelEnd])
          | (_, Ex ex) =>
          let
              val r = Temp.newtemp () (* suggested on page 162.*)
              val then' = unEx(thenExp)
          in
            Ex (T.ESEQ(seq [test' (labelThen, labelEnd)
                     , T.LABEL labelThen
                     , T.MOVE(T.TEMP r, then')
                     , T.LABEL labelEnd],T.TEMP r))
          end
    end

fun ifThenElse2IR (test, thenExp, elseExp) =
    let
        val test' = unCx test
        val labelThen = Temp.newLabel "if_then"
        val labelElse = Temp.newLabel "if_else"
        val labelJoin = Temp.newLabel "if_join"

    in
        case (test', thenExp, elseExp)
         of (_, Cx _, Cx _) =>
          let
             val then' = unCx(thenExp)
             val else' = unCx(elseExp)
          in
             Cx (fn(t, f) =>
                seq [test' (labelThen, labelElse)
                , T.LABEL labelThen
                , then' (t,f)
                , T.JUMP(T.NAME labelJoin, [labelJoin])
                , T.LABEL labelElse
                , else' (t,f)
                , T.LABEL labelJoin
                 ])
          end
          | (_, Ex _, Ex _) =>
          let
              val r = Temp.newtemp () (* suggested on page 162 *)
              val then' = unEx(thenExp)
              val else' = unEx(elseExp)
          in
          Ex (T.ESEQ( seq [test' (labelThen, labelElse)
             , T.LABEL labelThen
             , T.MOVE(T.TEMP r, then')
             , T.JUMP(T.NAME labelJoin, [labelJoin])
             , T.LABEL labelElse
             , T.MOVE(T.TEMP r, else')
             , T.LABEL labelJoin
             ] , T.TEMP r))
          end
          | (_, Nx _, _) =>
          let
              val then' = unNx(thenExp)
              val else' = unNx(elseExp)
          in
              Nx (seq [test' (labelThen, labelElse)
              , T.LABEL labelThen
              , then'
              , T.JUMP(T.NAME labelJoin, [labelJoin])
              , T.LABEL labelElse
              , else'
              , T.LABEL labelJoin])
          end
          | (_, _, Nx _) =>
          let
              val then' = unNx(thenExp)
              val else' = unNx(elseExp)
          in
              Nx (seq [test' (labelThen, labelElse)
              , T.LABEL labelThen
              , then'
              , T.JUMP(T.NAME labelJoin, [labelJoin])
              , T.LABEL labelElse
              , else'
              , T.LABEL labelJoin])
          end
          | (_, Cx _, Ex _) =>
            let
		val r = Temp.newtemp ()
		val then' = unEx(thenExp) (* May cause tangle of jumps *)
		val else' = unEx(elseExp)
            in
		Ex (T.ESEQ( seq [test' (labelThen, labelElse)
				, T.LABEL labelThen
				, T.MOVE(T.TEMP r, then')
				, T.JUMP(T.NAME labelJoin, [labelJoin])
				, T.LABEL labelElse
				, T.MOVE(T.TEMP r, else')
				, T.LABEL labelJoin
				] , T.TEMP r))
            end
          | (_, Ex _, Cx _) =>
	    let
		val r = Temp.newtemp ()
		val then' = unEx(thenExp)
		val else' = unEx(elseExp) (* May cause tangle of jumps *)
            in
		Ex (T.ESEQ( seq [test' (labelThen, labelElse)
				, T.LABEL labelThen
				, T.MOVE(T.TEMP r, then')
				, T.JUMP(T.NAME labelJoin, [labelJoin])
				, T.LABEL labelElse
				, T.MOVE(T.TEMP r, else')
				, T.LABEL labelJoin
				] , T.TEMP r))
            end
    (*          | (_, _, _) =>
            raise Bug "encountered thenBody and elseBody of different kinds"*)
    end

fun binop2IR (oper, left, right) =
  let
      val unleft = unEx(left)
      val unright = unEx(right)
  in
      Ex (T.BINOP(oper,unleft,unright))
  end

fun relop2IR (oper, left, right) =
  let
      val unleft = unEx(left)
      val unright = unEx(right)
  in
    Cx (fn (t,f) => T.CJUMP(oper,unleft,unright,t,f))
  end

fun intOp2IR (TAbs.PlusOp, left, right)     = binop2IR (T.PLUS, left, right)
  | intOp2IR (TAbs.MinusOp, left, right)    = binop2IR (T.MINUS, left, right)
  | intOp2IR (TAbs.TimesOp, left, right)    = binop2IR (T.MUL, left, right)
  | intOp2IR (TAbs.DivideOp, left, right)   = binop2IR (T.DIV, left, right)
  | intOp2IR (TAbs.ExponentOp, left, right) = Ex(F.externalCall("exponent", [unEx(left),unEx(right)]))
  | intOp2IR (TAbs.EqOp, left, right)       = relop2IR (T.EQ, left, right)
  | intOp2IR (TAbs.NeqOp, left, right)      = relop2IR (T.NE, left, right)
  | intOp2IR (TAbs.LtOp, left, right)       = relop2IR (T.LT, left, right)
  | intOp2IR (TAbs.LeOp, left, right)       = relop2IR (T.LE, left, right)
  | intOp2IR (TAbs.GtOp, left, right)       = relop2IR (T.GT, left, right)
  | intOp2IR (TAbs.GeOp, left, right)       = relop2IR (T.GE, left, right)


fun let2IR ([], body) = body
  | let2IR (decls, body) = Ex (T.ESEQ (seq (map unNx decls), unEx body))

fun eseq2IR [] = raise Bug "attempt to eseq2IR an empty sequence"
  | eseq2IR (exp :: exps) =
    let
        fun eseq2IR' exp [] = unEx exp
          | eseq2IR' exp (exp'::exps') =
            T.ESEQ (unNx exp, eseq2IR' exp' exps')
    in
        Ex (eseq2IR' exp exps)
    end

fun seq2IR [] =
    let
        val mtseqLabel = Temp.newLabel "mtseq"
    in
        Nx (T.LABEL mtseqLabel)
    end
  | seq2IR [exp] = exp
  | seq2IR (exp :: exps) = Nx (T.SEQ (unNx exp, unNx (seq2IR exps)))

fun string2IR str =
    let
        val label = Temp.newLabel "string"
    in
        addFrag (F.STRING (label, str));
        Ex (T.NAME label)
    end

val defaultStringIR = string2IR "DefaultString"

fun relopStr2IR (left, right, str) =
    Ex (F.externalCall (str, [unEx left, unEx right]))

fun stringOp2IR (TAbs.EqOp, l, r)  = relopStr2IR (l, r, "stringEqual")
  | stringOp2IR (TAbs.NeqOp, l, r) = relopStr2IR (l, r, "stringNotEq")
  | stringOp2IR (TAbs.LtOp, l, r)  = relopStr2IR (l, r, "stringLess")
  | stringOp2IR (TAbs.LeOp, l, r)  = relopStr2IR (l, r, "stringLessEq")
  | stringOp2IR (TAbs.GtOp, l, r)  = relopStr2IR (l, r, "stringGreater")
  | stringOp2IR (TAbs.GeOp, l, r)  = relopStr2IR (l, r, "stringGreaterEq")
  | stringOp2IR (_, _, _)     = raise Bug "illegal operation on strings"

fun while2IR (test, body, done) =
    let
        val test' = unCx test
        val body' = unNx body
        val labelTest = Temp.newLabel "while_test"
        val labelBody = Temp.newLabel "while_body"
    in
        Nx(seq [ T.LABEL labelTest
               , test'(labelBody,done)
               , T.LABEL labelBody
	             , body'
	             , T.JUMP(T.NAME labelTest, [labelTest])
               , T.LABEL done])
    end

fun for2IR (var, done, lo, hi, body) =
    let
        val var' = unEx var
        val lo' = unEx lo
        val hi' = unEx hi
        val body' = unNx body
        val loT = Temp.newtemp ()
        val hiT = Temp.newtemp ()
        val bodyL = Temp.newLabel "for_body"
        val nextL = Temp.newLabel "for_next"
	      val beginL = Temp.newLabel "for_begin"
    in
        Nx(seq [ T.MOVE(T.TEMP loT, lo')
               , T.MOVE(T.TEMP hiT, hi')
	             , T.CJUMP(T.GT, T.TEMP loT, T.TEMP hiT, done, beginL)
	             , T.LABEL beginL
               , T.MOVE(var',T.TEMP(loT))
	             , T.JUMP (T.NAME nextL, [nextL])
               , T.LABEL bodyL
               , body'
	             , T.MOVE(var', T.BINOP(T.PLUS, var', T.CONST(1)))
	             , T.LABEL nextL
	             , T.CJUMP(T.LE, var', T.TEMP(hiT), bodyL, done)
               , T.LABEL done
                ])
    end

fun funCall2IR ( toLevel as Level ({frame, parent}, _)
               , fromLevel
               , label
               , exps) =
    let
	val str = Symbol.name label
        val sl = followStaticLink parent fromLevel
        fun iter [] = []
        | iter(x::xs) = unEx(x)::iter(xs)
    in
	case parent of
	    Top => Ex (F.externalCall(str, (T.TEMP F.FP) :: iter(exps)))
	  | _   => Ex (T.CALL (T.NAME label, sl :: iter(exps)))
    end
  | funCall2IR (Top, _, _, _) =
    raise Bug "called function seems to have above-top-level context"

fun procCall2IR ( toLevel as Level ({frame, parent}, _)
                , fromLevel
                , label
                , exps) =
    let
	val str = Symbol.name label
        val sl = followStaticLink parent fromLevel
        fun iter [] = []
        | iter(x::xs) = unEx(x)::iter(xs)
    in
        case parent of Top
            =>  Nx (T.EXP (F.externalCall(str, (T.TEMP F.FP) :: iter(exps))))
         | _=>  Nx (T.EXP (T.CALL (T.NAME label, sl :: iter(exps))))
    end
  | procCall2IR (Top, _, _, _) =
    raise Bug "called procedure seems to have above-top-level context"

fun array2IR (size, init) =
  let
      val unsize = unEx(size)
      val uninit = unEx(init)
  in
      Ex (F.externalCall("initArray", [unsize,uninit]))
  end

fun record2IR explist =
    let
        val size = T.CONST (length explist)
        val r = Temp.newtemp ()
        val setup = T.MOVE ( T.TEMP r
			   , F.externalCall("allocRecord", [size]))
        fun step (exp, n) =
            T.MOVE (T.MEM(T.BINOP(T.PLUS, T.TEMP r, T.CONST (n * F.wordSize)))
                   , unEx exp)
        fun steps ([], n) = []
          | steps (e::es, n) = (step (e, n))::(steps (es, n+1))
    in
        Ex (T.ESEQ (seq (setup :: steps (explist, 0)), T.TEMP r))
    end

fun subscript2IR (array, offset) =
    let
        val offsetT = Temp.newtemp ()
        val arrayT = Temp.newtemp ()
        val addressT = Temp.newtemp ()
        val maxInxT = Temp.newtemp ()
        val negativeL = Temp.newLabel "subs_neg"
        val nonNegativeL = Temp.newLabel "subs_nneg"
        val overflowL = Temp.newLabel "subs_ovf"
        val noOverflowL = Temp.newLabel "subs_novf"
        val array' = unEx array (* address *)
        val offset' = unEx offset (* value *)
    in
	Ex ((T.ESEQ(seq [T.MOVE(T.TEMP offsetT,offset')
			, T.MOVE(T.TEMP arrayT,array')
			, T.MOVE(T.TEMP maxInxT,
				 (T.MEM(T.BINOP(T.MINUS,T.TEMP arrayT, T.CONST(F.wordSize))))) (* Casper vi kan ikke få det til at virke TODO *)
			, T.CJUMP(T.GE, T.TEMP offsetT, T.CONST(0), nonNegativeL, negativeL)
			, T.LABEL negativeL
			, T.EXP (F.externalCall("arrInxError", [T.TEMP offsetT]))
			, T.LABEL nonNegativeL
			, T.CJUMP(T.LT, T.TEMP offsetT, T.TEMP maxInxT,
				  noOverflowL, overflowL)
			, T.LABEL overflowL
			, T.EXP (F.externalCall("arrInxError", [T.TEMP offsetT]))
			, T.LABEL noOverflowL
			, T.MOVE(T.TEMP addressT,T.BINOP(T.PLUS,T.TEMP arrayT, T.BINOP(
						     T.MUL,T.TEMP offsetT,T.CONST(F.wordSize))))]
		   , T.MEM(T.TEMP addressT))))
    end

fun funEntryExit {level = Level ({frame, parent}, _), body = body} =
    let
        val body' = F.funEntryExit1 (frame, unEx body)
        val frag = F.PROC {body = body', frame = frame}
    in
        addFrag frag
    end
  | funEntryExit {level = Top, ...} =
    raise Bug "attempt to add function at top level"

fun procEntryExit {level = Level ({frame, parent}, _), body = body} =
    let
        val body' = F.procEntryExit1 (frame, unNx body)
        val frag = F.PROC {body = body', frame = frame}
    in
        addFrag frag
    end
  | procEntryExit {level = Top, ...} =
    raise Bug "attempt to add procedure at top level"

fun getResult () = getFrags ()

(* ---------- Printing ---------- *)

fun asStringLevel Top = Symbol.name Temp.topLabel
  | asStringLevel (Level ({frame, parent}, _)) =
    "Level {frame = " ^
    F.asStringFrame frame ^
    ", parent = " ^
    asStringLevel parent ^
    "}"

fun asStringAccess (level, access) =
    "(" ^
    asStringLevel level ^
    ", " ^
    F.asStringAccess access ^
    ")"

fun asStringExp (Ex t) = "Ex(" ^ PT.asStringExp t ^ ")"
  | asStringExp (Nx t) = "Nx(" ^ PT.asStringStm t ^ ")"
  | asStringExp (Cx f) =
    let
        val labelTrue = Temp.newLabel "true_label"
        val labelFalse = Temp.newLabel "false_label"
    in
        "Cx(" ^ PT.asStringStm (f (labelTrue, labelFalse)) ^ ")"
    end

fun printX asStringX (outstream, x) =
    ( TextIO.output (outstream, asStringX x ^ "\n")
    ; TextIO.flushOut outstream)

val printLevel  = printX asStringLevel
val printAccess = printX asStringAccess
val printExp    = printX asStringExp

end (* Translate *)
