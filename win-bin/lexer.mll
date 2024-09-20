        {
        open Parser    
        open String     
        exception Eof
        }
        rule token = parse
         (*   [""]     { token lexbuf }  *)
          | ['\n' ]        { EOL }
          | [  '=' '!' '^' '%' '$' '>' '<' '?' '[' ']' '#'] as c { SPE(c) }
            | '+'  {PLUS}
            | '-'  {MINUS}
             | ' '  {SPACE}
          | ['a'-'z']+ as s  { LOW(s)  }
          |'{'   {BEGIN}
          | '}'   {END}
          | ['A'-'Z']+ as s  { UPP(s)  }
          | ['0'-'9']+ as lxm { DIG(int_of_string lxm) }
          | "-|"{ACT1}
          | "|-" {ACT2}
          | eof  { raise Eof } 
 