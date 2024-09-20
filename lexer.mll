{
  (* Define the tokens your parser will use *)
  type token =
    | TRANSACTION
    | OBJECT
    | VERIFY
    | EXEC
    | IDENT of string
    | INT of int
    | FLOAT of float
    | HEX of int
    | BIN of int
    | STRING of string
    | COMMENT
    | PLUS | MINUS | MULT | DIV
    | EQ | NEQ | LEQ | GEQ
    | AND | OR
    | ASSIGN
    | LBRACKET | RBRACKET
    | LBRACE | RBRACE
    | LPAREN | RPAREN
    | TYPE | MATCH | RETURN | IF | WHILE | FOR
    | FUNC  (* Use func for functions *)
    | VOID
    | PARENT | WRITE | DOT  (* New keywords and operator *)
    | EOF
}

rule read = parse
  (* Keywords *)
  | "transaction" { TRANSACTION }
  | "object" { OBJECT }
  | "verify" { VERIFY }
  | "exec" { EXEC }
  | "int" { INT }
  | "float" { FLOAT }
  | "bool" { BOOL }
  | "string" { STRING }
  | "vector" { VECTOR }
  | "void" { VOID }
  | "type" { TYPE }
  | "match" { MATCH }
  | "return" { RETURN }
  | "if" { IF }
  | "while" { WHILE }
  | "for" { FOR }
  | "func" { FUNC }  (* Function keyword changed to func *)
  | "parent" { PARENT }  (* New keyword *)
  | "write" { WRITE }    (* New keyword *)

  (* Identifiers *)
  | ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as id { IDENT id }

  (* Numbers: integers, floating-point, hexadecimal, and binary *)
  | ['0'-'9']+ as int_lit { INT (int_of_string int_lit) }
  | ['0'-'9']+\.[0-9]*([eE][+\-]?[0-9]+)? as float_lit { FLOAT (float_of_string float_lit) }
  | "0x"['0'-'9' 'a'-'f' 'A'-'F']+ as hex_lit { HEX (int_of_string hex_lit) }
  | "0b"['0'-'1']+ as bin_lit { BIN (int_of_string bin_lit) }

  (* String literals *)
  | '"' [^ '"' '\\' '\n']* '"' as str_lit { STRING (String.sub str_lit 1 (String.length str_lit - 2)) }

  (* Single-line comments *)
  | "//" [^ '\n']* { COMMENT }

  (* Multi-line comments with support for nesting *)
  | "/*" { comment 1 }

  (* Operators *)
  | "+" { PLUS }
  | "-" { MINUS }
  | "*" { MULT }
  | "/" { DIV }
  | "==" { EQ }
  | "!=" { NEQ }
  | "<=" { LEQ }
  | ">=" { GEQ }
  | "&&" { AND }
  | "||" { OR }
  | "=" { ASSIGN }

  (* Brackets and parentheses *)
  | "[" { LBRACKET }
  | "]" { RBRACKET }
  | "{" { LBRACE }
  | "}" { RBRACE }
  | "(" { LPAREN }
  | ")" { RPAREN }

  (* Dot operator for object property access *)
  | "." { DOT }   (* New operator *)

  (* Whitespace and newlines are ignored *)
  | [' ' '\t' '\n']+ { read lexbuf }

  (* End of file *)
  | eof { EOF }

(* This handles nested comments *)
and comment depth = parse
  | "/*" { comment (depth + 1) }
  | "*/" when depth = 1 { read lexbuf }
  | "*/" { comment (depth - 1) }
  | _ { comment depth }
