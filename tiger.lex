(* -*- mode:sml -*- *)

type svalue = Tokens.svalue
type pos = int
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult = (svalue, pos) token

val inString = ref false
val lineNum = ErrorMsg.lineNum
val linePos = ErrorMsg.linePos

val commentLevel = ref 0
val currentString = ref ""

(* Final code *)
val pad3 = StringCvt.padLeft #"0" 3
val toOct = Int.fmt StringCvt.OCT
fun dec2oct n = pad3 (toOct n)

fun err (p1,p2) = ErrorMsg.error p1

fun eof () =
    let
        val pos = hd (!linePos)
    in
    if !commentLevel <> 0 then (ErrorMsg.error pos "unclosed comment")
      else if !inString = true then (inString := false; (ErrorMsg.error pos "unclosed string"))
      else (); commentLevel := 0; Tokens.EOF (pos,pos)
    end

fun s2i t pos =
    let
        val opti = (Int.fromString t)
            handle Overflow =>
                   (ErrorMsg.error pos "Integer too large"; SOME 0)
        fun s2i_aux NONE = (ErrorMsg.error pos "Ill-formed integer"; 0)
          | s2i_aux (SOME n) = n
    in
        s2i_aux opti
    end

fun dopos token yypos yylen = token (yypos, yypos + yylen)
fun dopos3 token value yypos yylen = token (value, yypos, yypos + yylen)

%%

%header (functor TigerLexFun (structure Tokens: Tiger_TOKENS));

letter=[a-zA-Z];
digits=[0-9]+;
idchars=[a-zA-Z][a-zA-Z0-9_]*;
ignore=[\t\ \n]+;
control=\^[@A-Z\\_\^];
ascii=0[0-9][0-9]|1[0-9][0-9]|2[0-4][0-9]|25[0-5];
%s COMMENT STRING ESCAPE IGNORE CONTROL;

%%



<INITIAL,COMMENT>"\n"	            => (lineNum := !lineNum+1;
                                        linePos := yypos :: !linePos;
                                        continue());
<INITIAL>"\t"                       => (continue());
<INITIAL>","                        => (dopos Tokens.COMMA yypos 1);
<INITIAL>"var"                      => (dopos Tokens.VAR yypos 3);
<INITIAL>"if"                       => (dopos Tokens.IF yypos 2);
<INITIAL>"then"                     => (dopos Tokens.THEN yypos 4);
<INITIAL>"else"                     => (dopos Tokens.ELSE yypos 4);
<INITIAL>"function"                 => (dopos Tokens.FUNCTION yypos 8);
<INITIAL>"break"                    => (dopos Tokens.BREAK yypos 5);
<INITIAL>"of"                       => (dopos Tokens.OF yypos 2);
<INITIAL>"end"                      => (dopos Tokens.END yypos 3);
<INITIAL>"in"                       => (dopos Tokens.IN yypos 2);
<INITIAL>"nil"                      => (dopos Tokens.NIL yypos 3);
<INITIAL>"let"                      => (dopos Tokens.LET yypos 3);
<INITIAL>"do"                       => (dopos Tokens.DO yypos 2);
<INITIAL>"to"                       => (dopos Tokens.TO yypos 2);
<INITIAL>"for"                      => (dopos Tokens.FOR yypos 3);
<INITIAL>"while"                    => (dopos Tokens.WHILE yypos 5);
<INITIAL>"array"                    => (dopos Tokens.ARRAY yypos 5);
<INITIAL>"type"                     => (dopos Tokens.TYPE yypos 4);
<INITIAL>":="                       => (dopos Tokens.ASSIGN yypos 2);
<INITIAL>"|"                        => (dopos Tokens.OR yypos 2);
<INITIAL>"&"                        => (dopos Tokens.AND yypos 3);
<INITIAL>">"                        => (dopos Tokens.GT yypos 1);
<INITIAL>">="                       => (dopos Tokens.GE yypos 2);
<INITIAL>"<="                       => (dopos Tokens.LE yypos 2);
<INITIAL>"<"                        => (dopos Tokens.LT yypos 1);
<INITIAL>"<>"                       => (dopos Tokens.NEQ yypos 2);
<INITIAL>"="                        => (dopos Tokens.EQ yypos 1);
<INITIAL>"/"                        => (dopos Tokens.DIVIDE yypos 1);
<INITIAL>"*"                        => (dopos Tokens.TIMES yypos 1);
<INITIAL>"-"                        => (dopos Tokens.MINUS yypos 1);
<INITIAL>"+"                        => (dopos Tokens.PLUS yypos 1);
<INITIAL>"."                        => (dopos Tokens.DOT yypos 1);
<INITIAL>"}"                        => (dopos Tokens.RBRACE yypos 1);
<INITIAL>"{"                        => (dopos Tokens.LBRACE yypos 1);
<INITIAL>"]"                        => (dopos Tokens.RBRACK yypos 1);
<INITIAL>"["                        => (dopos Tokens.LBRACK yypos 1);
<INITIAL>")"                        => (dopos Tokens.RPAREN yypos 1);
<INITIAL>"("                        => (dopos Tokens.LPAREN yypos 1);
<INITIAL>";"                        => (dopos Tokens.SEMICOLON yypos 1);
<INITIAL>":"                        => (dopos Tokens.COLON yypos 1);
<INITIAL>"&"                        => (dopos Tokens.AND yypos 1);
<INITIAL>"|"                        => (dopos Tokens.OR yypos 1);
<INITIAL>"^"                        => (dopos Tokens.CARET yypos 1);
<INITIAL>" "                        => (continue());
<INITIAL>"/*"                       => (commentLevel := !commentLevel + 1; YYBEGIN COMMENT; continue());



<COMMENT>"/*"                       => (commentLevel := !commentLevel + 1; continue());
<COMMENT>"*/"                       => (commentLevel := !commentLevel - 1;
                                        if (!commentLevel = 0)
                                        then (YYBEGIN INITIAL; continue())
                                        else continue());
<COMMENT>.                          => (continue());

<INITIAL>\"                         => (currentString := ""; inString := true; YYBEGIN STRING; continue());

<STRING>\"                          => (inString := false; YYBEGIN INITIAL;
                                        dopos3 Tokens.STRING (!currentString) yypos (size(!currentString)));


<STRING>\\                          => (YYBEGIN ESCAPE; continue());
<STRING>\n                          => (ErrorMsg.error yypos ("Newlines should be put into ignore escape sequence in string"); continue());
<STRING>.                           => (currentString := !currentString ^ yytext; continue());

<ESCAPE>n                           => (currentString := !currentString ^ "\n"; YYBEGIN STRING;
					                              lineNum := !lineNum+1;
                                        linePos := yypos :: !linePos;  (* After feedback *)
                                        continue());
<ESCAPE>t                           => (currentString := !currentString ^ "\t"; YYBEGIN STRING; continue());
<ESCAPE>\"                          => (currentString := !currentString ^ yytext; YYBEGIN STRING; continue());
<ESCAPE>\\                          => (currentString := !currentString ^ "\\"; YYBEGIN STRING; continue());
<ESCAPE>{ascii}                     => (currentString := !currentString ^ valOf(String.fromString("\\" ^ yytext)); YYBEGIN STRING; continue());
<ESCAPE>{control}                   => (currentString := !currentString ^ valOf(String.fromString("\\" ^ yytext)); YYBEGIN STRING; continue());
<ESCAPE>{ignore}                    => (YYBEGIN IGNORE; continue());
<ESCAPE>.                           => (ErrorMsg.error yypos ("Invalid escape character " ^ yytext); YYBEGIN STRING; continue());


<IGNORE>"\\n"                       => (lineNum := !lineNum+1;
                              					linePos := yypos :: !linePos;
                              					continue());
<IGNORE>"\\t"                       => (continue());
<IGNORE>"\\f"                       => (continue());
<IGNORE>"\\"{control}               => (continue());
<IGNORE>"\\"                        => (YYBEGIN STRING; continue());
<IGNORE>.                           => (ErrorMsg.error yypos ("Invalid character in ignore. No support for " ^ yytext); continue());


<INITIAL>{digits}                   => (dopos3 Tokens.INT (s2i yytext yypos) yypos
                                                 (size yytext));
<INITIAL>{idchars}                  => (dopos3 Tokens.ID yytext yypos (size yytext));

.                                   => (ErrorMsg.error yypos ("illegal char " ^ yytext);
                                        continue());
