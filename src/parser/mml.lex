(* mml.lex
 *
 * COPYRIGHT (c) 2024 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Spring 2024
 * University of Chicago
 *
 * ML-Ulex specification for MiniML.
 *)

%name MMLLex;

%arg (lexErr);

%defs(
    structure T = MMLTokens

    (* some type lex_result is necessitated by ml-ulex *)
    type lex_result = T.token

    (* starting position of current block comment or string; used for error reporting *)
    val startPos : Error.pos ref = ref 0

    (* list of string fragments to concatenate *)
    val buf : string list ref = ref []

    (* add a string to the buffer *)
    fun addStr s = (buf := s :: !buf)

    (* make a string from buf *)
    fun mkString () = (T.STRING(String.concat(List.rev(!buf))) before buf := [])

    (* keyword lookup table *)
    local
      val find =
          let val tbl = AtomTable.mkTable (32, Fail "keywords")
              fun ins (id, tok) = AtomTable.insert tbl (Atom.atom id, tok)
          in
              app ins [
                ("case",        T.KW_case),
                ("do",          T.KW_do),
                ("else",        T.KW_else),
                ("end",         T.KW_end),
                ("fun",         T.KW_fun),
                ("if",          T.KW_if),
                ("in",          T.KW_in),
                ("let",         T.KW_let),
                ("of",          T.KW_of),
                ("then",        T.KW_then),
                ("type",        T.KW_type)
              ];
              AtomTable.find tbl
          end
    in
    (* return either a keyword token or an ID token *)
    fun idToken id = let
          val ida = Atom.atom id
          in
            case find ida
             of NONE => T.LID ida
              | SOME kw => kw
            (* end case *)
          end
    end

    (* support for nested comments *)
    val commentDepth = ref 0
    fun startComment () = (commentDepth := !commentDepth + 1)
    fun endComment () = let
          val d = !commentDepth-1
          in
            commentDepth := d;
            (d <= 0)
          end
);

%states INITIAL COM STRING;

%let ucLetter = [A-Z];
%let lcLetter = [a-z];
%let letter = {ucLetter}|{lcLetter};
%let dig = [0-9];
%let idchar = {letter}|{dig}|"_"|"'";
%let ucId = {ucLetter}{idchar}*;
%let lcId = {lcLetter}{idchar}*;
%let esc = "\\"[abfnrtv\\\"]|"\\"{dig}{dig}{dig};
%let sgood = [\032-\126]&[^\"\\]; (* sgood means "characters good inside strings" *)
%let ws = " "|[\t\n\v\f\r];
%let eol = "\n"|"\r\n"|"\r";

<INITIAL> "/*"          => (YYBEGIN COM;
                            startPos := yypos;
                            startComment();
                            skip());
<INITIAL> "("           => (T.LP);
<INITIAL> ")"           => (T.RP);
<INITIAL> "["           => (T.LB);
<INITIAL> "]"           => (T.RB);
<INITIAL> "{"           => (T.LCB);
<INITIAL> "}"           => (T.RCB);
<INITIAL> "||"          => (T.ORELSE);
<INITIAL> "&&"          => (T.ANDALSO);
<INITIAL> ":="          => (T.ASSIGN);
<INITIAL> "=="          => (T.EQEQ);
<INITIAL> "!="          => (T.NEQ);
<INITIAL> "<="          => (T.LTE);
<INITIAL> "<"           => (T.LT);
<INITIAL> "+"           => (T.PLUS);
<INITIAL> "-"           => (T.MINUS);
<INITIAL> "*"           => (T.TIMES);
<INITIAL> "/"           => (T.DIV);
<INITIAL> "%"           => (T.MOD);
<INITIAL> "!"           => (T.BANG);
<INITIAL> "="           => (T.EQ);
<INITIAL> ","           => (T.COMMA);
<INITIAL> ":"           => (T.COLON);
<INITIAL> "|"           => (T.BAR);
<INITIAL> "->"          => (T.ARROW);
<INITIAL> "=>"          => (T.DARROW);
<INITIAL> "_"           => (T.WILD);
<INITIAL> {lcId}        => (idToken yytext);
<INITIAL> {ucId}        => (T.UID(Atom.atom yytext));
<INITIAL> {dig}+        => (T.NUMBER(valOf (IntInf.fromString yytext)));
<INITIAL> {ws}          => (skip ());
<INITIAL> "//"[^\n\r]*{eol}  (* end-of-line comment *)
                        => (skip ());
<INITIAL> "\""          => (YYBEGIN STRING; startPos := yypos; continue());
<INITIAL>.              => (lexErr((yypos, yypos), ["bad character `", String.toString yytext, "'"]);
                            skip());
<INITIAL> <<EOF>>       => (T.EOF);

(* string lexing *)
<STRING> {esc}          => (if (yytext = "\\000")
                              then lexErr((yypos, yypos+4), [
                                  "illegal escape '\\000' in string literal"
                                ])
                              else case String.fromString yytext
                                 of SOME s => addStr s
                                  | NONE => lexErr((yypos, yypos+4), [
                                        "illegal escape '", yytext, "' in string literal"
                                      ])
                                (* end case *);
                            continue());
<STRING> {sgood}+       => (addStr yytext; continue());
<STRING> "\""           => (YYBEGIN INITIAL; mkString());
<STRING> "\\".          => (lexErr((yypos, yypos), [
                                "bad escape character `", String.toString yytext,
                                "' in string literal"
                              ]);
                            continue());
<STRING> \n|\r|\r\n     => (lexErr((!startPos, yypos), ["unclosed string at end of line"]);
                            YYBEGIN INITIAL; mkString());
<STRING> .              => (lexErr((yypos, yypos), [
                                "bad character `", String.toString yytext,
                                "' in string literal"
                              ]);
                            skip());
<STRING> <<EOF>>        => (lexErr((!startPos, yypos), ["unclosed string at end of file"]);
                            T.EOF);

(* block-comment lexing *)
<COM> "/*"              => (startComment(); skip());
<COM> "*/"              => (if endComment() then YYBEGIN INITIAL else (); skip());
<COM> <<EOF>>           => (lexErr((!startPos, yypos), ["unclosed comment at end of file"]);
                            T.EOF);
<COM> .                 => (skip());
