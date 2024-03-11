# Owl Language Reference

## Comments
    
Comments are any line starting with `#`, or a pair of `#*` ... `*#`

## Arguments

Arguments are identifier with optional type information

    arg ::= <ident>
        |   <ident>: <type>

    arglist ::= <arg>
        |   <arg>, <arglist>

## Declarations

Statements are either declarations, or expressions that end with a semicolon

    stmt ::= let <arg> = <expr>;
        | <expr>;
        | fun <ident>(<arglist>) { <stmt>* }
        | fun <ident>(<arglist>) -> <type> { <stmt>* }

## Types

    <type> ::= int | bool | unit
        | (<type>)
        | <type> -> <type>

## Expressions

    <atom> ::= () | true | false
        | <ident> 
        | <number> 
        | (<expr>)

    <expr> ::= <atom>
        |   <atom> <op> <expr>
        |   <atom>(<exprlist>)

    <exprlist> ::= <expr> 
        | <expr>, <exprlist>
