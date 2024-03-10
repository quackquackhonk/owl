# Owl Language Reference

## Comments
    
Comments are any line starting with `#`, or a pair of `#*` ... `*#`

## Argument

Arguments are identifier with optional type information

    arg ::= <ident>
        |   <ident>: <type>

    arglist ::= <arg>
        |   <arg>, <arglist>

## Statements

Statements are either declarations, or expressions that end with a semicolon

    stmt ::= let <arg> = <expr>;
        |   fun <ident>(<arglist>) {
                (<stmt> | <expr>)+
            }
        |   fun <ident>(<arglist>) -> <type> {
                (<stmt> | <expr>)+
            }
        |   <expr>;

## Types

    <type> ::= int | bool | unit
        |   <type> -> <type>

## Expressions

    <atom> ::= <ident> 
        | <number> 
        | true 
        | false 
        | () 
        | (<expr>)

    <expr> ::= <atom>
        |   <atom> <op> <expr>
        |   <atom>(<exprlist>)

    <exprlist> ::= <expr> 
        | <expr>, <exprlist>
