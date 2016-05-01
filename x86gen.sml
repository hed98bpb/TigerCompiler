signature CODEGEN =
sig
    structure Frame: FRAME
    val codegen: Frame.frame -> Tree.stm -> Assem.instr list
end

structure X86Gen: CODEGEN =
struct

structure Frame: FRAME = X86Frame
structure S = Symbol
structure T = Tree
structure Tm = Temp
structure A = Assem
structure F = Frame
structure PT = PrintTree(F)

exception TODO

fun int i =
    if i >= 0
    then Int.toString i
    else "-" ^ Int.toString (~i)

fun codegen frame stm =
    let
        val ilist = ref (nil: A.instr list)

        fun emit x = (ilist := x :: (!ilist))

        fun result gen =
            let val t = Tm.newtemp ()
            in  gen t; t
            end

        fun operator2jump oper =
            case oper of
                T.EQ => "je"
              | T.NE => "jne"
              | T.LT => "jl"
              | T.GT => "jg"
              | T.LE => "jle"
              | T.GE => "jge"
              | _ => "bad branch operator!"

        fun moveInstr s d doc = A.MOVE { assem = "\tmovl `s0, `d0"
                                       , src = s
                                       , dst = d
                                       , doc = "x86gen:" ^ doc}

        fun adjustSP count = A.OPER { assem = "\taddl $" ^ int count ^ ", `d0"
                                    , src = [F.SP] (* old-SP used *)
                                    , dst = [F.SP]
                                    , jump = NONE
                                    , doc = "x86gen:55"}

        fun allocArgs count = adjustSP (~F.wordSize*count)
        fun freeArgs count = adjustSP (F.wordSize*count)

        fun munchStm (T.SEQ (a, b)) = (munchStm a; munchStm b)

          (* MOVE *)
          | munchStm (T.MOVE (T.TEMP t, T.CALL (T.NAME l, args))) =
            ( emit (A.OPER { assem = "\tcall " ^ S.name l
                           , src = munchArgs args
                           , dst = F.calldefs
                           , jump = NONE
                           , doc = "x86gen:68"})
            ; emit (freeArgs (length args))
            ; emit (moveInstr F.EAX t "70"))

          | munchStm (T.MOVE (T.MEM e1, T.CALL (T.NAME l, args))) =
            let
                val e1' = munchExp e1
            in
               (emit (A.OPER { assem = "\tcall " ^ S.name l
                             , src = munchArgs args
                             , dst = F.calldefs
                             , jump = NONE
                             , doc = "x86gen:80"})
              ; emit (freeArgs (length args))
              ; emit (A.OPER { assem = "\tmovl `s0, (`s1)"
                      , src = [F.EAX, munchExp e1]
                      , dst = []
                      , jump = NONE
                      , doc = "x86gen:86"}))
            end

          | munchStm (T.MOVE (T.MEM (T.BINOP (T.PLUS, e1, T.CONST i)), e2)) =
            ( emit (A.OPER { assem = "\tmovl `s1, " ^ int i ^ "(`s0)"
                             , src = [munchExp e1, munchExp e2]
                             , dst = []
                             , jump = NONE
                             , doc = "x86gen:94" }))

          | munchStm (T.MOVE (T.MEM (T.BINOP (T.PLUS, T.CONST i, e1)), e2)) =
            ( emit (A.OPER { assem = "\tmovl `s1, " ^ int i ^ "(`s0)"
                             , src = [munchExp e1, munchExp e2]
                             , dst = []
                             , jump = NONE
                             , doc = "x86gen:101" }))

          | munchStm (T.MOVE (T.MEM (T.CONST i), e2)) =
          let
              val t = Tm.newtemp ()
          in
            ( emit (A.OPER { assem = "\tmovl " ^ int i ^ "`d0"
                             , src = []
                             , dst = [t]
                             , jump = NONE
                             , doc = "x86gen:111"})
              ; emit (A.OPER { assem = "\tmovl `s0, (`s1)"
                             , src = [munchExp e2, t]
                             , dst = []
                             , jump = NONE
                             , doc = "x86gen:116" })) (* Hopefully we'll never use this case *)
          end
          | munchStm (T.MOVE (T.MEM e1, e2)) =
            ( emit (A.OPER { assem = "\tmovl `s1, (`s0)"
                             , src = [munchExp e1, munchExp e2]
                             , dst = []
                             , jump = NONE
                             , doc = "x86gen:123" }))

          | munchStm (T.MOVE (T.TEMP i, e2)) =
            ( emit (moveInstr (munchExp e2) i  "126"))

          | munchStm (T.LABEL lab) =
            ( emit (A.LABEL { assem = S.name lab ^ ":"
                              , lab = lab
                              , doc = "x86gen:131" }))

          (* JUMP *)
          | munchStm (T.CJUMP (oper, T.CONST i, e2, lab1, lab2)) =
            let
              val lab = Tm.newLabel "fallthrough"
              val t = Tm.newtemp()
            in
              ( emit (A.OPER {assem = "\tmovl $" ^ int i ^", `d0"
                              , src = []
                              , dst = [t]
                              , jump = NONE
                              , doc = "x86gen:143"})
              ; emit (A.OPER {assem = "\tcmpl `s0, `s1"
                              , src = [munchExp e2, t]
                              , dst = []
                              , jump = NONE
                              , doc = "x86gen:148" })
              ; emit (A.OPER {assem = "\t" ^(operator2jump(oper)) ^ " `j0"
                              , src = []
                              , dst = []
                              , jump = SOME [lab1, lab]
                              , doc = "x86gen:153" })
              ; emit (A.LABEL {assem = S.name lab ^ ":" (* fallthough label *)
                              , lab = lab
                              , doc = "x86gen:156"})
              ; emit (A.OPER {assem = "\tjmp `j0"
                              , src = []
                              , dst = []
                              , jump = SOME [lab2]
                              , doc = "x86gen:161" }) )
            end

          | munchStm (T.CJUMP (oper, e1, T.CONST i, lab1, lab2)) =
            let
              val lab = Tm.newLabel "fallthrough"
            in
              ( emit (A.OPER {assem = "\tcmpl $" ^ int i ^ ", `s0"
                            , src = [munchExp e1]
                            , dst = []
                            , jump = NONE
                            , doc = "x86gen:172" })
              ; emit (A.OPER {assem = "\t" ^(operator2jump(oper)) ^" `j0"
                            , src = []
                            , dst = []
                            , jump = SOME [lab1]
                            , doc = "x86gen:177" })
              ; emit (A.LABEL {assem = S.name lab ^ ":"
                            , lab = lab
                            , doc = "x86gen:180"})
              ; emit (A.OPER {assem = "\tjmp `j0"
                            , src = []
                            , dst = []
                            , jump = SOME [lab2]
                            , doc = "x86gen:185" }) )
            end

          | munchStm (T.CJUMP (oper, e1, e2, lab1, lab2)) =
            let
              val lab = Tm.newLabel "fallthrough"
            in
              ( emit (A.OPER {assem = "\tcmpl `s1, `s0"
                            , src = [munchExp e1, munchExp e2]
                            , dst = []
                            , jump = NONE
                            , doc = "x86gen:196" })
                ; emit (A.OPER {assem = "\t" ^ (operator2jump(oper)) ^" `j0"
                              , src = []
                              , dst = []
                              , jump = SOME [lab1]
                              , doc = "x86gen:201" })
                ; emit (A.LABEL {assem = S.name lab ^ ":"
                              , lab = lab
                              , doc = "x86gen:204"})
                ; emit (A.OPER {assem = "\tjmp `j0"
                              , src = []
                              , dst = []
                              , jump = SOME [lab2]
                              , doc = "x86gen:209" }) )
            end

          | munchStm (T.JUMP (T.NAME lab, llst)) =
            ( emit (A.OPER {assem = "\tjmp "^ S.name lab
                          , src = []
                          , dst = []
                          , jump = SOME llst
                          , doc = "x86gen:217" }))

          (* EXP *)
          | munchStm (T.EXP (T.CALL (T.NAME lab, args))) =
            ( emit (A.OPER {assem = "\tcall " ^ S.name lab
                          , src = munchArgs args
                          , dst = F.calldefs
                          , jump = NONE
                          , doc = "x86gen:225"})
            ; emit (freeArgs (length args)))

          | munchStm (T.EXP exp) =
            ( munchExp exp; ())

          (* If no match so far, complain *)
          | munchStm (T.JUMP a) =
            emit (A.OPER { assem = "\t# JUMP: bad JUMP in munchStm!"
                         , src = []
                         , dst = []
                         , jump = NONE
                         , doc = "x86gen:237"})

          | munchStm (T.MOVE a) =
            emit (A.MOVE { assem = "\t# MOVE: bad MOVE in munchStm!"
                         , src = Tm.newtemp ()
                         , dst = Tm.newtemp ()
                         , doc = "x86gen:243"})

        and munchArgs args =
            (* in the simple approach used here, we pass all args in memory *)
            let val rargs = rev args (* push args right-to-left *)
                fun munchArgs1 [] = []
                  | munchArgs1 (ah::at) =
                       (emit (A.OPER {assem = "\tpushl `s0"
                                    , src = [munchExp ah]
                                    , dst = []
                                    , jump = NONE
                                    , doc = "x86gen:254"});munchArgs1(at)) (* we already have temps pushed to stack, so doesn't really matter what we output *)
            in  munchArgs1 rargs
            end

        (* Memory access *)
        and munchExp (T.MEM (T.BINOP (T.PLUS, e, T.CONST n))) =
            result (fn r => emit (A.OPER { assem = "\tmovl " ^ int n ^
                                                   "(`s0), `d0"
                                         , src = [munchExp e]
                                         , dst = [r]
                                         , jump = NONE
                                         , doc = "x86gen:265"}))

          | munchExp (T.MEM (T.BINOP (T.PLUS, T.CONST n, e))) =
            result (fn r => emit (A.OPER { assem = "\tmovl " ^ int n ^
                                                   "(`s0), `d0"
                                         , src = [munchExp e]
                                         , dst = [r]
                                         , jump = NONE
                                         , doc = "x86gen:273"}))

          | munchExp (T.MEM (T.BINOP (T.MINUS, e, T.CONST n))) =
            result (fn r => emit (A.OPER { assem = "\tmovl " ^ int (~n) ^
                                                   "(`s0), `d0"
                                         , src = [munchExp e]
                                         , dst = [r]
                                         , jump = NONE
                                         , doc = "x86gen:281"}))
          | munchExp (T.MEM e) =
            result (fn r => emit (A.OPER { assem = "\tmovl (`s0), `d0"
                                           , src = [munchExp e]
                                           , dst = [r]
                                           , jump = NONE
                                           , doc = "x86gen:287"}))

          (* PLUS *)
          | munchExp (T.BINOP (T.PLUS, e1, T.CONST i)) =
            result (fn r => (emit (moveInstr (munchExp e1) r "291")
                           ; emit (A.OPER { assem = "\taddl $" ^ int i ^ ", `s0"
                                          , src = [r]
                                          , dst = [r]
                                          , jump = NONE
                                          , doc = "x86gen:296"})))

          | munchExp (T.BINOP (T.PLUS, T.CONST i, e1)) =
            result (fn r => (emit (moveInstr (munchExp e1) r "299")
                           ; emit (A.OPER { assem = "\taddl $" ^ int i ^ ", `s0"
                                          , src = [r]
                                          , dst = [r]
                                          , jump = NONE
                                          , doc = "x86gen:304"})))

          | munchExp (T.BINOP (T.PLUS, e1, e2)) =
            (* Hint, p203: use src=[r,_] and do not use `s0,
             * which specifies that r is used *)
            result (fn r => (emit (moveInstr (munchExp e1) r "309")
                          ; emit (A.OPER { assem = "\taddl `s1, `d0"
                                         , src = [r, munchExp e2]
                                         , dst = [r]
                                         , jump = NONE
                                         , doc = "x86gen:314"})))

          (* MINUS *)
          | munchExp (T.BINOP (T.MINUS, e1, T.CONST i)) =
            result (fn r => (emit (moveInstr (munchExp e1) r "318")
                           ; emit (A.OPER { assem = "\tsubl $" ^ int i ^ ", `s0"
                                          , src = [r]
                                          , dst = [r]
                                          , jump = NONE
                                          , doc = "x86gen:323"})))

          | munchExp (T.BINOP (T.MINUS, T.CONST 0, e1)) =
            result (fn r => (emit (A.OPER { assem = "\tmovl $" ^ int 0 ^ ", `d0"
                                         , src = []
                                         , dst = [r]
                                         , jump = NONE
                                         , doc = "x86gen:330"})
                           ; emit (A.OPER { assem = "\tsubl `s1, `d0"
                                         , src = [r, munchExp e1]
                                         , dst = [r]
                                         , jump = NONE
                                         , doc = "x86gen:335"})))

          | munchExp (T.BINOP (T.MINUS, T.CONST i, e1)) =
            result (fn r => (emit (A.OPER { assem = "\tmovl $" ^ int i ^ ", `d0"
                                          , src = []
                                          , dst = [r]
                                          , jump = NONE
                                          , doc = "x86gen:342"})
                           ; emit (A.OPER { assem = "\tsubl `s1, `d0"
                                          , src = [r, munchExp e1]
                                          , dst = [r]
                                          , jump = NONE
                                          , doc = "x86gen:347"})))

          | munchExp (T.BINOP (T.MINUS, e1, e2)) =
            result (fn r => (emit (moveInstr (munchExp e1) r "350")
                           ; emit (A.OPER { assem = "\tsubl `s1, `d0"
                                          , src = [r, munchExp e2]
                                          , dst = [r]
                                          , jump = NONE
                                          , doc = "x86gen:355"})))

          (* MULTIPLY *)
          | munchExp (T.BINOP (T.MUL, e1, e2)) =
            result (fn r => (emit (moveInstr (munchExp e1) r "359")
                          ; (emit (A.OPER { assem = "\timull `s1, `d0"
                                         , src = [r, munchExp e2]
                                         , dst = [r]
                                         , jump = NONE
                                         , doc = "x86gen:364"}))))

          (* DIVIDE *)

          | munchExp (T.BINOP (T.DIV, e1, e2)) =
            (* Hint from
             * http://www.cs.mun.ca/~rod/winter2004/cs3724/notes/asm.html:
             *
             * "To divide x by y, we first convert it into 64-bits, and
             * them use idivl.
             *
             *  movl  x, %eax
             *  cltd
             *  idivl y
             *
             * The quotient is in %eax, and the remainder is in %edx."
             *)

            let
              val e1 = munchExp e1
              val e2 = munchExp e2
            in
            result (fn r => (emit (moveInstr e2 r "386")
                           ; emit (moveInstr e1 F.EAX "387")
                           ; emit (A.OPER { assem = "\tcltd"
                                          , src = []
                                          , dst = []
                                          , jump = NONE
                                          , doc = "x86gen:392"})
                           ; emit (A.OPER { assem = "\tidivl `s0"
                                          , src = [r]
                                          , dst = []
                                          , jump = NONE
                                          , doc = "x86gen:397"})
                           ; emit (moveInstr F.EAX r "398") ))
            end

          (* AND *) (* All of these statements have been rewritten to if-then-else *)
          | munchExp (T.BINOP (T.AND, e1, T.CONST i)) =
            result (fn r => (emit (A.OPER { assem = "\tmovl $" ^ int i ^ ", `d0"
                                          , src = []
                                          , dst = [r]
                                          , jump = NONE
                                          , doc = "x86gen:407"})
                           ; emit (A.OPER { assem = "\tandl `s1, `d0"
                                          , src = [r, munchExp e1]
                                          , dst = [r]
                                          , jump = NONE
                                          , doc = "x86gen:412"})))

          | munchExp (T.BINOP (T.AND, T.CONST i, e1)) =
            result (fn r => (emit (A.OPER { assem = "\tmovl $" ^ int i ^ ", `d0"
                                          , src = []
                                          , dst = [r]
                                          , jump = NONE
                                          , doc = "x86gen:419"})
                           ; emit (A.OPER { assem = "\tandl `s1, `d0"
                                          , src = [r, munchExp e1]
                                          , dst = [r]
                                          , jump = NONE
                                          , doc = "x86gen:424"})))

          | munchExp (T.BINOP (T.AND, e1, e2)) =
            result (fn r => (emit (moveInstr (munchExp e1) r "427")
                           ; emit (A.OPER { assem = "\tandl `s1, `d0"
                                          , src = [r, munchExp e2]
                                          , dst = [r]
                                          , jump = NONE
                                          , doc = "x86gen:432"})))

          (* OR *)
          | munchExp (T.BINOP (T.OR, e1, T.CONST i)) =
            result (fn r => (emit (A.OPER { assem = "\tmovl $" ^ int i ^ ", `d0"
                                          , src = []
                                          , dst = [r]
                                          , jump = NONE
                                          , doc = "x86gen:440"})
                           ; emit (A.OPER { assem = "\torl `s1, `d0"
                                          , src = [r, munchExp e1]
                                          , dst = [r]
                                          , jump = NONE
                                          , doc = "x86gen:445"})))

          | munchExp (T.BINOP (T.OR, T.CONST i, e1)) =
            result (fn r => (emit (A.OPER { assem = "\tmovl $" ^ int i ^ ", `d0"
                                          , src = []
                                          , dst = [r]
                                          , jump = NONE
                                          , doc = "x86gen:452"})
                           ; emit (A.OPER { assem = "\torl `s1, `d0"
                                          , src = [r, munchExp e1]
                                          , dst = [r]
                                          , jump = NONE
                                          , doc = "x86gen:457"})))

          | munchExp (T.BINOP (T.OR, e1, e2)) =
            result (fn r => (emit (moveInstr (munchExp e1) r "460")
                           ; emit (A.OPER { assem = "\torl `s1, `d0"
                                          , src = [r, munchExp e2]
                                          , dst = [r]
                                          , jump = NONE
                                          , doc = "x86gen:465"}))) (* Unnecessary, since we use if-statements instead *)

          (* Other constructs *)
          | munchExp (T.TEMP t) = t

          | munchExp (T.ESEQ (s, e)) = (munchStm s; munchExp e) (* Should never happen *)

          | munchExp (T.NAME label) =
            result (fn r =>
                    emit (A.OPER { assem = "\tmovl $" ^ S.name(label) ^ ", `d0"
                                 , src = []
                                 , dst = [r]
                                 , jump = NONE
                                 , doc = "x86gen:478"}))

          | munchExp (T.CONST n) =
            result (fn r =>
                    emit (A.OPER { assem = "\tmovl $" ^ int n ^ ", `d0"
                                  , src = []
                                  , dst = [r]
                                  , jump = NONE
                                  , doc = "x86gen:486"}))

          (* If no match so far, complain *)
          | munchExp (tr as T.CALL (_, _)) =
            ( TextIO.output (TextIO.stdErr, "\nBUG: bad CALL in munchExp:\n")
            ; PT.printExp (TextIO.stdErr, tr)
            ; TextIO.flushOut TextIO.stdErr
            ; result (fn _ => emit (A.OPER { assem = "\t# bad CALL!"
                                           , src = []
                                           , dst = []
                                           , jump = NONE
                                           , doc = "x86gen:497"})))

          | munchExp (tr as T.BINOP (_, _, _)) =
            ( TextIO.output (TextIO.stdErr, "\nBUG: bad BINOP in munchExp:\n")
            ; PT.printExp (TextIO.stdErr, tr)
            ; TextIO.flushOut TextIO.stdErr
            ; result (fn _ => emit (A.OPER { assem = "\t# bad BINOP!"
                                           , src = []
                                           , dst = []
                                           , jump = NONE
                                           , doc = "x86gen:507"})))
    in
        munchStm stm;
        rev (!ilist)
    end

end (* X86Gen *)
