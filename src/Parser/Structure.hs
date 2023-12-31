{-
-- EPITECH PROJECT, 2023
-- Apollo2
-- File description:
-- Structure.hs
-}

module Parser.Structure (module Parser.Structure) where

import Ast.Ast (Ast (..), Operable (..), Operation (CallStd), Structure (..), Type (..))
import Control.Applicative (Alternative ((<|>)), optional)
import Eval.Atom (Atom (AtomI))
import Eval.Operator (Operator (Add, Concat, Div, Mod, Mul, Sub))
import Parser.Ast (parseAst)
import Parser.Char (parseAChar, parseChar, parseClosingCurlyBraquet, parseClosingParenthesis, parseNotAnyChar, parseOpeningCurlyBraquet, parseOpeningParenthesis)
import Parser.Error (replaceErr)
import Parser.Operable (parseDefinitionName, parseElement)
import Parser.Operation (checkOperator)
import Parser.Position (Position (..))
import Parser.Range (Range (..))
import Parser.StackTrace (StackTrace (..), defaultLocation)
import Parser.Symbol (parseSymbol, parseSymbolType, parseType)
import Parser.Syntax (parseMany, parseManyStructure, parseMaybeparenthesis, parseWithSpace)
import Parser.Type (Parser (..))

----------------------------------------------------------------

parseSpecificInstruction :: Parser String
parseSpecificInstruction =
  parseWithSpace
    ( parseSymbol "return"
        <|> parseSymbol "if"
        <|> parseSymbol "while"
        <|> parseSymbol "for"
    )

makeStack :: String -> Position -> Position -> StackTrace
makeStack str p pos = StackTrace [(str, Range p pos, defaultLocation)]

msg1Err :: String -> String
msg1Err inst = "Syntax error: instruction " ++ inst ++ " is not valid"

msg2Err :: String
msg2Err = "Syntax error: invalid instruction or bad assignation"

parseError :: Parser Ast
parseError = Parser $ \s p ->
  case runParser (parseWithSpace parseSymbolType) s p of
    Right (_, _, ps) ->
      Left (makeStack "Syntax error: bad variable definition" p ps)
    Left _ -> case runParser parseSpecificInstruction s p of
      Right (inst, _, pos) -> Left (makeStack (msg1Err inst) p pos)
      Left _ -> case runParser (parseWithSpace (parseChar '}')) s p of
        Right _ -> Left (makeStack "" p p)
        Left _ -> Left (makeStack msg2Err p p)

parseAstStructure :: Parser Ast
parseAstStructure =
  parseWithSpace $
    AstStructure
      <$> ( parseVarDefinition
              <|> parseVarAssignation
              <|> parseReturn
              <|> parseIf
              <|> parseWhile
              <|> parseFor
          )

----------------------------------------------------------------

parseStringWithHandleBackslash :: Parser String
parseStringWithHandleBackslash =
  replaceErr
    "Syntax error: bad return"
    (parseMany (parseChar '\\' *> parseAChar <|> parseAChar))

----------------------------------------------------------------

createVarDef :: Parser Type -> Parser String -> Parser (Maybe Operable) -> Parser Structure
createVarDef parType parStr op = Parser $ \s p -> case runParser parType s p of
  Right (typ, str, pos) -> case runParser parStr str pos of
    Right (name, string, position) -> case runParser op string position of
      Right (ope, new_str, new_pos) ->
        Right ((VarDefinition name typ ope), new_str, new_pos)
      Left a -> Left a
    Left a -> Left a
  Left a -> Left a

parseVarDefinition :: Parser Structure
parseVarDefinition =
  createVarDef
    (parseType <* parseChar ' ')
    (parseWithSpace parseDefinitionName)
    (optional (parseWithSpace (parseChar '=') *> parseElement))
    <* parseChar ';'

----------------------------------------------------------------

getIncrement :: String -> Maybe Operator
getIncrement "++" = Just Add
getIncrement "--" = Just Sub
getIncrement _ = Nothing

parseIncrement :: Parser String
parseIncrement =
  parseSymbol "++"
    <|> parseSymbol "--"

parseIncrementOp :: String -> Parser Operable
parseIncrementOp name = Parser $ \s p ->
  case runParser parser s p of
    Right (operand, newstr, newpos) ->
      Right (OpOperation $ CallStd operand sugar, newstr, newpos)
    Left a -> Left a
  where
    sugar = [OpVariable name, OpValue $ AtomI 1]
    parser = parseWithSpace $ checkOperator parseIncrement getIncrement

getOpequality :: String -> Maybe Operator
getOpequality "+=" = Just Add
getOpequality "-=" = Just Sub
getOpequality "*=" = Just Mul
getOpequality "/=" = Just Div
getOpequality "%=" = Just Mod
getOpequality ":=" = Just Concat
getOpequality _ = Nothing

parseOpEquality :: Parser String
parseOpEquality =
  parseSymbol "+="
    <|> parseSymbol "-="
    <|> parseSymbol "*="
    <|> parseSymbol "/="
    <|> parseSymbol "%="
    <|> parseSymbol ":="

parseEqualityOp :: String -> Parser Operable
parseEqualityOp name =
  (\operand eleme -> OpOperation $ CallStd operand [OpVariable name, eleme])
    <$> parseWithSpace (checkOperator parseOpEquality getOpequality)
    <*> parseMaybeparenthesis parseElement

parseVarEquality :: Parser Structure
parseVarEquality =
  VarAssignation
    <$> parseWithSpace parseDefinitionName
    <*> ( parseWithSpace (parseChar '=')
            *> parseElement
            <* parseWithSpace (parseChar ';')
        )

parseVarIncrementation :: Parser Structure
parseVarIncrementation = Parser $ \s p ->
  case runParser (parseWithSpace parseDefinitionName) s p of
    Right (name, str2, pos2) ->
      case runParser (op_par name) str2 pos2 of
        Right (op, str3, pos3) -> Right (VarAssignation name op, str3, pos3)
        Left a -> Left a
    Left a -> Left a
  where
    op_par name =
      parseWithSpace (parseIncrementOp name) <* parseWithSpace (parseChar ';')

parseVarOperation :: Parser Structure
parseVarOperation = Parser $ \s p ->
  case runParser (parseWithSpace parseDefinitionName) s p of
    Right (name, str2, pos2) ->
      case runParser (op_par name) str2 pos2 of
        Right (op, str3, pos3) -> Right (VarAssignation name op, str3, pos3)
        Left a -> Left a
    Left a -> Left a
  where
    op_par name =
      parseWithSpace (parseEqualityOp name) <* parseWithSpace (parseChar ';')

parseVarAssignation :: Parser Structure
parseVarAssignation =
  parseVarEquality
    <|> parseVarIncrementation
    <|> parseVarOperation

----------------------------------------------------------------

parseReturnWithParenthesis :: Parser Operable
parseReturnWithParenthesis =
  parseWithSpace (parseSymbol "return")
    *> parseOpeningParenthesis
    *> parseElement
    <* parseWithSpace parseClosingParenthesis

parseReturnWithoutParenthesis :: Parser Operable
parseReturnWithoutParenthesis =
  parseWithSpace (parseSymbol "return") *> parseElement

parseReturn :: Parser Structure
parseReturn =
  Return
    <$> ( (parseReturnWithParenthesis <|> parseReturnWithoutParenthesis)
            <* parseChar ';'
        )

----------------------------------------------------------------

parsecond :: Parser Operable
parsecond =
  parseWithSpace
    ( parseMaybeparenthesis
        ( parseWithSpace parseElement
        )
    )

parseIf :: Parser Structure
parseIf =
  (\then' elifs -> If (then' : elifs))
    <$> ((,) <$> parseWithSpace (parseSymbol "if" *> parsecond) <*> parseThen)
    <*> parseMany parseElIf
    <*> parseElse

parseThen :: Parser Ast
parseThen =
  parseWithSpace parseOpeningCurlyBraquet
    *> (AstStructure <$> parseWithSpace parseSequence)
    <* parseWithSpace parseClosingCurlyBraquet

parseElIf :: Parser (Operable, Ast)
parseElIf =
  (,)
    <$> parseWithSpace (parseSymbol "elif" *> parsecond)
    <*> parseThen

parseElse :: Parser (Maybe Ast)
parseElse = Parser $ \s p ->
  case runParser (parseWithSpace $ parseSymbol "else") s p of
    Right (_, newstr, newpos) -> case runParser parseThen newstr newpos of
      Right (thn, thnstr, thnpos) -> Right (Just thn, thnstr, thnpos)
      Left a -> Left a
    Left _ -> Right (Nothing, s, p)

----------------------------------------------------------------

parseWhile :: Parser Structure
parseWhile =
  While <$> parseWithSpace (parseSymbol "while" *> parsecond) <*> parseThen

parseFor :: Parser Structure
parseFor =
  For
    <$> parseWithSpace
      (parseSymbol "for" *> parseWithSpace parseDefinitionName)
    <*> parseWithSpace (parseSymbol "in" *> parseElement)
    <*> parseThen

----------------------------------------------------------------

parseSingle :: Parser Structure
parseSingle = Single <$> parseAst

findStruct :: Parser String
findStruct =
  parseWithSpace
    ( parseSymbolType
        <|> parseSymbol "return"
        <|> parseSymbol "if"
        <|> parseSymbol "else"
    )

findNewStruc :: Parser String
findNewStruc =
  Parser $ \s p -> case runParser findStruct s p of
    Right _ -> Right ("", s, p)
    Left (StackTrace [("Not Found: End of Input", ran, src)]) ->
      Left (StackTrace [("", ran, src)])
    Left a -> Left a

findSequenceEnd :: Parser String
findSequenceEnd = Parser $ \s p ->
  case runParser (parseChar '}') s p of
    Right _ -> Left (StackTrace [("", Range p p, defaultLocation)])
    Left a -> Left a

findNextInstructionOrSequence :: Parser String
findNextInstructionOrSequence = Parser $ \s p ->
  case runParser (parseWithSpace (parseNotAnyChar [';', '{'])) s p of
    Right (_, str, pos) -> runParser findNextInstruction str pos
    Left (StackTrace [("Not Found: List is empty", ran, src)]) ->
      Left (StackTrace [("", ran, src)])
    Left _ -> case runParser parseAChar s p of
      Right (a, str, pos) -> Right ([a], str, pos)
      Left (StackTrace [(_, ran, src)]) -> Left (StackTrace [("", ran, src)])
      Left a -> Left a

findNextInstruction :: Parser String
findNextInstruction = Parser $ \s p -> case runParser findNewStruc s p of
  Right a -> Right a
  Left (StackTrace [("", ran, src)]) -> Left (StackTrace [("", ran, src)])
  Left _ -> case runParser findSequenceEnd s p of
    Left (StackTrace [("", ran, src)]) -> Left (StackTrace [("", ran, src)])
    _ -> runParser findNextInstructionOrSequence s p

moveToError :: Position -> Parser String
moveToError ps = Parser $ \s p ->
  if p == ps
    then Right ("", s, p)
    else case runParser parseAChar s p of
      Right (_, str, pos) -> runParser (moveToError ps) str pos
      Left a -> Left a

parseNextInstruction :: Parser [Ast] -> Range -> String -> Parser [Ast]
parseNextInstruction parser (Range p1 p2) err = Parser $ \s p ->
  case runParser (nextP p2) s p of
    Right _ -> Left (StackTrace [(err, Range p1 p2, defaultLocation)])
    Left (StackTrace [("", Range _ p3, _)]) ->
      Left (StackTrace [(err, Range p1 p3, defaultLocation)])
    Left (StackTrace ys) ->
      Left (StackTrace (ys ++ [(err, Range p1 p2, defaultLocation)]))
  where
    nextP pos =
      moveToError pos *> findNextInstruction *> parseManyInstructions parser

parseManyInstructions :: Parser [Ast] -> Parser [Ast]
parseManyInstructions parser = Parser $ \s p -> case runParser parser s p of
  Right a -> Right a
  Left (StackTrace [(xs, ran, _)]) ->
    runParser (parseNextInstruction parser ran xs) s p
  Left a -> Left a

parseManyAst :: Parser [Ast]
parseManyAst =
  parseManyInstructions
    ( parseManyStructure (parseError <|> parseAst)
    )

parseSequence :: Parser Structure
parseSequence = Sequence <$> parseManyAst
