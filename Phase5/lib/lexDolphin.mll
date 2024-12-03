{
    open Parser
    exception Unrecognized (* your code should eventually compile without this exception *)
}

let line_comment = "//" [^ '\n']+
let digits = ['0'-'9']+
let idents = ['a'-'z'] | ['A'-'Z'] | '_'
let ident = idents (digits | idents)* (* Test _number works*)
let str_start = '"' | '{'
let str = ident+ | digits

rule token = parse
| ' ' { token lexbuf }
| '\n' { Lexing.new_line lexbuf; token lexbuf }
| line_comment {token lexbuf }
| "/*" { comment 0 lexbuf }
| "int" { INT }
| "bool" { BOOL }
| "void" { VOID }
| '+' { PLUS }
| '-' { MINUS }
| "new" { NEW }
| '*' { MUL }
| '%' { REM }
| '/' { DIV }
| '(' { LPAREN }
| ')' { RPAREN }
| "var" { VAR }
| '=' { ASSIGN }
| "==" { EQ }
| ';' { SEMICOLON }
| '"' str '"' as s { STRING_LIT s }
| '{' str '}' as s {STRING_LIT s }
| "true" { TRUE }
| "false" { FALSE }
| "length_of" { LENGTHOF }
| "record" { RECORD }
| "<" { LT }
| "<=" { LE }
| ">" { GT }
| ">=" { GE }
| "||" { LOR }
| "&&" { LAND }
| '!' { LNOT }
| "!=" { NEQ }
| '?' { QUESTIONMARK }
| ':' { COLON }
| ',' { COMMA }
| ';' { SEMICOLON }
| '.' { DOT }
| '[' { LBRACKET }
| ']' { RBRACKET }
| '{' { LBRACE }
| '}' { RBRACE }
| "nil" { NIL }
| "var" { VAR }
| "let" { LET }
| "if" { IF }
| "else" { ELSE }
| "while" { WHILE }
| "for" { FOR }
| "break" { BREAK }
| "continue" { CONTINUE }
| "return" { RETURN }
| "string" { STRING }
| ident as s { IDENT (s) }
| digits as i_lit { INT_LIT (Int64.of_string i_lit) }
| eof { EOF }
| _ {  raise Unrecognized }

and comment nesting_lvl = parse 
    | "/*" { comment (nesting_lvl + 1) lexbuf }
    | "*/" {
        if nesting_lvl = 0 then
            token lexbuf
        else 
            comment (nesting_lvl - 1) lexbuf
    }
    | '\n' {Lexing.new_line lexbuf; comment nesting_lvl lexbuf}
    | eof { raise Unrecognized }
    | _ { comment nesting_lvl lexbuf }
