# Syntax

## Comments

Comments are any line starting with `#`, or a pair of `#*` ... `*#`

## Arguments

Arguments are identifier with optional type information

    <arg> ::= <ident>
        |   <ident>::<type>

## Declarations

There are value and function declarations

    <return> ::= -> <type>

    <decl> ::= let <arg> = <expr>;
        | fun <ident> <arg>* <return>? = <expr>;
        | fun <ident> <arg>* <return>? { <block> }

Statements are either declarations, or expressions that end with a semicolon

    <stmt> ::= <decl> | <expr>;

## Types

    <type> ::= int | bool | unit
        | (<type>)
        | <type> -> <type>

## Expressions

    <expr> ::= <call>
        | <call> <op> <expr>

    <call> ::= <atom>
        | <atom> <call>

    <atom> ::= () | true | false
        | <ident>
        | <number>
        | (<expr>)
        | { <block> }

    <block> ::= <stmt>* <expr>?

# Semantics

# Tooling
