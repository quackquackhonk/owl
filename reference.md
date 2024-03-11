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

There are value and function declarations

    <return> ::= -> <type>

    decl ::= let <arg> = <expr>;
        | fun <ident>(<arglist>) <return>? = <expr>;
        | fun <ident>(<arglist>) <return>? { <block> }

Statements are either declarations, or expressions that end with a semicolon

    stmt ::= <decl> | <expr>;

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
        | <atom> <op> <expr>
        | <atom>(<exprlist>)
        | { <block> }

    <exprlist> ::= <expr> 
        | <expr>, <exprlist>

    <block> ::= <stmt>* <expr>?
