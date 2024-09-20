 %{   open List
    open Ast %}

   %token EOL 
   %token SPACE
   %token ACT1 ACT2
   %token <char> SPE
   %token <int> DIG
   %token <string> LOW
   %token <string> UPP
   %token PLUS MINUS
      %token BEGIN END
   %start pwdf
   %type <unit> pwdf


   %%
   pwdf: funct  { () };
 

   plus: PLUS { ( ) } 
   | SPACE plus { ( ) }
   | plus SPACE { ( ) }
   ;
   minus : MINUS { ( ) }
   | SPACE minus { ( ) }
   | minus SPACE { ( ) };
   operation: 
   | pwd plus pwd { '+',(  $1 , $3  )}
   | pwd minus pwd { '-',(  $1 ,  check_pw $3) }
   | SPACE operation { $2 }
   | operation SPACE { $1 }
   ;


   pwd: SPE {   [Ast.Special $1] }
 
   | DIG {  [Ast.Digit $1]   } 
   | LOW {  [Ast.Lowercase $1] }
   | UPP {   [Ast.Uppercase $1]  }
   | pwd DIG {  ($1 @ [Ast.Digit $2])    }
   | pwd LOW {   ( $1 @ [Ast.Lowercase $2])    }
   | pwd UPP {  ($1 @ [Ast.Uppercase $2])    }
   | pwd SPE {   ($1 @ [Ast.Special $2])   }
   | ACT1 ACT2 {    Ast.Iterator::(!(Ast.active_pw))    }
   |ACT1 pwd ACT2 {  Ast.Iterator::(if List.is_empty (!(Ast.active_pw)) then $2 else !(Ast.active_pw))  }
   ;  
    funct :
   | BEGIN sequence END { Ast.run_sequence $2 None}
   | BEGIN DIG sequence END { Ast.run_sequence $3 (Some $2) }
   ;
   sequence:
   |  operation   { [$1] }
      |  SPACE sequence   {  $2  }
   | sequence SPACE {  $1  }
   | EOL sequence {  $2  }
   |sequence EOL  {  $1  }
   | sequence EOL sequence {  $1  @  $3  }
 
