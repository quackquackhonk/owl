import Syntax.AST
import qualified Data.ByteString.Lazy as B
import Syntax.Parser
import System.IO
import Test.HUnit

examples =
  TestList
    [ TestLabel "Fibonacci Function" testFibonacci,
      TestLabel "Identifiers" testIdentifiers,
      TestLabel "Precedence" testPrecedence,
      TestLabel "Sequences" testSequences,
      TestLabel "Currying Functions" testCurrying,
      TestLabel "Logical Operators" testLogicalOperators
      -- TestLabel "Anonymous Functions" testAnonFunctions
    ]

makeExampleTest name expected = TestCase $ do
  contents <- B.readFile $ "examples/" ++ name
  assertEqual name expected (astFromStr contents)

testFibonacci = makeExampleTest "fibonacci.owl" fibAst
  where
    fibAst = Program decls expr
    decls =
      [ DValue (Name "fib") [Name "n"] $
          ECond
            (mkELtEq (mkEVar "n") (EInt 1))
            (EInt 1)
            ( mkEAdd
                (ECall (mkEVar "fib") (EParen $ mkESub (mkEVar "n") (EInt 1)))
                (ECall (mkEVar "fib") (EParen $ mkESub (mkEVar "n") (EInt 2)))
            )
      ]
    expr = ECall (mkEVar "fib") (EInt 10)

testIdentifiers = makeExampleTest "identifiers.owl" $ Program decs expr
  where
    decs =
      [ DValue (Name "identifier") [] $ EInt 10,
        DValue (Name "something?") [] $ EBool True,
        DValue (Name "identifier'") [] $ mkEVar "identifier",
        DValue (Name "a_long_word") [] $ EInt 12345678900,
        DValue (Name "__dunder__") [] $ mkEVar "something?"
      ]
    expr = mkEVar "a_long_word"

testPrecedence = makeExampleTest "precedence.owl" $ Program decs EUnit
  where
    decs =
      [ DValue (Name "_") [] (mkEAdd (mkEAdd (mkEAdd (EInt 1) (EInt 2)) (EInt 3)) (EInt 4)),
        DValue (Name "_") [] (mkEAdd (mkEAdd (EInt 1) (EInt 2)) (mkEMul (EInt 3) (EInt 4))),
        DValue (Name "id") [Name "x"] (mkEVar "x"),
        DValue (Name "_") [] (mkEAdd (ECall (mkEVar "id") (EInt 2)) (EInt 1)),
        DValue (Name "_") [] (ECall (mkEVar "id") $ mkEAdd (EInt 2) (EInt 1)),
        DValue (Name "_") [] (ECond (EBool True) (EInt 1) (mkEAdd (EInt 2) (EInt 3))),
        DType (Name "sum") (TArrow TInt (TArrow TInt (TArrow TInt TInt))),
        DValue (Name "sum") [Name "x", Name "y", Name "z"] (mkEAdd (mkEAdd (mkEVar "x") (mkEVar "y")) (mkEVar "z"))
      ]

testSequences = makeExampleTest "sequences.owl" $ Program decs EUnit
  where
    decs =
      [ DValue
          (Name "complex")
          [Name "w", Name "x", Name "y", Name "z"]
          ( ESeq
              [ Decl $ DValue (Name "a") [] (mkESub (mkEVar "w") (mkEVar "x")),
                Decl $ DValue (Name "b") [] (mkEDiv (mkEVar "y") (mkEVar "z")),
                Decl $ DValue (Name "c") [] (mkEMul (mkEVar "a") (mkEVar "b")),
                Expr $ EUnit,
                Expr $ EBool True,
                Expr $ mkEVar "c"
              ]
          )
      ]

testLogicalOperators = makeExampleTest "logical_operations.owl" $ Program decs exp
  where
    decs = [DValue (Name "_") [] (mkEOr (EBool True) (mkEAnd (EBool False) (EBool True)))]
    exp =
      mkEOr
        (mkEOr (mkEEq (EInt 1) (EInt 2)) (mkEAnd (mkENeq (EInt 3) (EInt 5)) (EUnaryOp UONot $ EBool False)))
        (mkELtEq (EUnaryOp UONeg $ EInt 7) (mkESub (EInt 2) (EInt 6)))

testCurrying = makeExampleTest "currying.owl" $ Program decs exp
  where
    decs =
      [ DType (Name "sum4") $ repeatFuncType 5 TInt,
        DValue (Name "sum4") [Name "a", Name "b", Name "c", Name "d"] $
          mkEAdd (mkEAdd (mkEAdd (mkEVar "a") (mkEVar "b")) (mkEVar "c")) (mkEVar "d"),
        DType (Name "sum3") $ repeatFuncType 4 TInt,
        DValue (Name "sum3") [] $
          ECall (mkEVar "sum4") (EInt 1),
        DType (Name "sum1") (TArrow TInt TInt),
        DValue (Name "sum1") [] $
          ECall (ECall (mkEVar "sum3") (EInt 2)) (mkEAdd (EInt 5) (EInt 5))
      ]
    exp = EBinaryOp BOEq (EInt 20) (ECall (mkEVar "sum1") (EInt 7))

testAnonFunctions = makeExampleTest "anonymous_functions.owl" $ Program decs exp
  where
    decs =
      [ DValue (Name "foo") [] $
          EFunc [Name "x", Name "y"] $
            EBinaryOp BOMul (EVar $ Name "x") (mkEVar "y"),
        DValue (Name "ten") [] $ ECall (ECall (mkEVar "foo") (EInt 2)) (EInt 5),
        DValue (Name "num") [] (EInt 10),
        DValue (Name "numPlus") [] $
          EFunc [Name "x"] (EBinaryOp BOAdd (mkEVar "num") (mkEVar "x")),
        DValue (Name "makeAdder") [Name "x"] $
          EFunc [Name "y"] (mkEAdd (mkEVar "x") (mkEVar "y"))
      ]
    exp =
      ESeq
        [Decl $ DValue (Name "numPlus'") [] (ECall (mkEVar "makeAdder") (EInt 20))]

main = runTestTT examples
