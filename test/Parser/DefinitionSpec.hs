{-
-- EPITECH PROJECT, 2023
-- Dev_repo2
-- File description:
-- Definition.hs
-}

module Parser.DefinitionSpec (definitionTests) where

import Test.HUnit

import Parser.Definition
import Parser.Position (defaultPosition)
import Parser.PositionSpec (getPosition)
import Parser.Type (Parser (..))

import Ast.Ast (Ast (..), Function (..), Structure (..), Operation (..), Type (..), Definition (..), Operable (..))
import Eval.Atom (Atom (..))
import Eval.Operator (Operator(..))
import Ast.Display (red, resetColor, yellow, green)


definitionTests :: Test
definitionTests =
    TestList
    [ "parseFuncDefinition" ~: parseFuncDefinitionTests
    ]

left :: String
left = "Left " ++ red ++ "Error found during parsing:\n" ++ resetColor

displaypos :: String -> String
displaypos p = green ++ p ++ resetColor

startAndFinish :: String -> String -> String
startAndFinish p1 p2 = " started at " ++ displaypos p1 ++ " and finished at " ++ displaypos p2 ++ "\n"

err :: String -> String
err str = yellow ++ str ++ resetColor

invalidInstruction :: Test
invalidInstruction =
    TestList
    [ "Invalid return" ~: (left ++ "\tin \"foo\"(" ++ (displaypos "0:0")  ++ "): " ++ (err "Syntax error: instruction return is not valid") ++ (startAndFinish "1:0" "1:7")) @=? (show (runParser parseFuncDefinition "@foo() int {\nreturn ;}\n" defaultPosition)),
      "Invalid if" ~: (left ++ "\tin \"foo\"(" ++ (displaypos "0:0")  ++ "): " ++ (err "Syntax error: instruction if is not valid") ++ (startAndFinish "1:0" "1:3")) @=? (show (runParser parseFuncDefinition "@foo() int {\nif (b = 2) {}}\n" defaultPosition)),
      "Invalid for" ~: (left ++ "\tin \"foo\"(" ++ (displaypos "0:0")  ++ "): " ++ (err "Syntax error: instruction for is not valid") ++ (startAndFinish "1:0" "1:4")) @=? (show (runParser parseFuncDefinition "@foo() int {\nfor i i range {}}\n" defaultPosition)),
      "Invalid while" ~: (left ++ "\tin \"foo\"(" ++ (displaypos "0:0")  ++ "): " ++ (err "Syntax error: instruction while is not valid") ++ (startAndFinish "1:0" "1:6")) @=? (show (runParser parseFuncDefinition "@foo() int {\nwhile (i ! 0) {}}\n" defaultPosition))
    ]

parseFuncDefinitionTests :: Test
parseFuncDefinitionTests =
    TestList
    [ "Valid" ~: ("Right " ++ (show (FuncDefinition "foo" (Function [("i", TypeInt),("c",TypeChar),("b", TypeBool),("f", TypeFloat)] Nothing (AstStructure (Sequence []))),"", getPosition 39 0))) @=? (show (runParser parseFuncDefinition "@foo(int i, char c, bool b, float f) {}" defaultPosition)),
      "Valid return" ~: ("Right " ++ (show (FuncDefinition "foo" (Function [] (Just TypeInt) (AstStructure (Sequence [AstStructure (Return (OpValue (AtomI 4)))]))),"", getPosition 1 2))) @=? (show (runParser parseFuncDefinition "@foo() int {\nreturn 4;\n}" defaultPosition)),
      "Valid if" ~: ("Right " ++ (show (FuncDefinition "foo" (Function [] (Just TypeInt) (AstStructure (Sequence [AstStructure (If [(OpOperation (CallStd Eq [OpVariable "b",OpValue (AtomI 2)]),AstStructure (Sequence [AstStructure (Return (OpValue (AtomI 4)))]))] Nothing)]))),"", getPosition 1 2))) @=? (show (runParser parseFuncDefinition "@foo() int {\nif (b == 2) {return 4;}\n}" defaultPosition)),
      "Valid for" ~: ("Right " ++ (show (FuncDefinition "foo" (Function [] (Just TypeInt) (AstStructure (Sequence [AstStructure (For "i" (OpVariable "range") (AstStructure (Sequence [])))]))),"", getPosition 1 2))) @=? (show (runParser parseFuncDefinition "@foo() int {\nfor i in range {}\n}" defaultPosition)),
      "Valid for" ~: ("Right " ++ (show (FuncDefinition "foo" (Function [] (Just TypeInt) (AstStructure (Sequence [AstStructure (While (OpOperation (CallStd NEq [OpVariable "i",OpValue (AtomI 0)])) (AstStructure (Sequence [])))]))),"", getPosition 1 2))) @=? (show (runParser parseFuncDefinition "@foo() int {\nwhile (i != 0) {}\n}" defaultPosition)),
      "Error in name" ~: ( left ++ "\tin \"\"(" ++ (displaypos "0:0")  ++ "): " ++ (err "Not Found: char is not '@' (is 'f')") ++ (startAndFinish "0:0" "0:0")) @=? (show (runParser parseFuncDefinition "foo() int {\nreturn 4;\n}" defaultPosition)),
      "Error in parameters" ~: (left ++ "\tin \"foo\"(" ++ (displaypos "0:0")  ++ "): " ++ (err "Not Found: Missing closing Parenthesis") ++ (startAndFinish "0:5" "0:5")) @=? (show (runParser parseFuncDefinition "@foo(in c) int {\nreturn 4;\n}" defaultPosition)),
      "Error opening parenthesis" ~: (left ++ "\tin \"foo\"(" ++ (displaypos "0:0")  ++ "): " ++ (err "Not Found: Missing opening Parenthesis") ++ (startAndFinish "0:4" "0:4")) @=? (show (runParser parseFuncDefinition "@foo) int {\nreturn 4;\n}" defaultPosition)),
      "Error closing parenthesis" ~: (left ++ "\tin \"foo\"(" ++ (displaypos "0:0")  ++ "): " ++ (err "Not Found: Missing closing Parenthesis") ++ (startAndFinish "0:5" "0:5")) @=? (show (runParser parseFuncDefinition "@foo( int {\nreturn 4;\n}" defaultPosition)),
      "Error returning type" ~: (left ++ "\tin \"foo\"(" ++ (displaypos "0:0")  ++ "): " ++ (err "Not Found: Missing opening curlybraquet") ++ (startAndFinish "0:7" "0:7")) @=? (show (runParser parseFuncDefinition "@foo() it {\nreturn 4;\n}" defaultPosition)),
      "Error opening curly bracket" ~: (left ++ "\tin \"foo\"(" ++ (displaypos "0:0")  ++ "): " ++ (err "Not Found: Missing opening curlybraquet") ++ (startAndFinish "1:0" "1:0")) @=? (show (runParser parseFuncDefinition "@foo() int \nreturn 4;\n}" defaultPosition)),
      "Error opening curly bracket" ~: (left ++ "\tin \"foo\"(" ++ (displaypos "0:0")  ++ "): " ++ (err "Not Found: Missing closing curlybraquet") ++ (startAndFinish "2:0" "2:0")) @=? (show (runParser parseFuncDefinition "@foo() int {\nreturn 4;\n" defaultPosition)),
      invalidInstruction
    ]
