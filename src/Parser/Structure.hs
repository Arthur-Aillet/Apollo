{-
-- EPITECH PROJECT, 2023
-- Dev_repo2
-- File description:
-- Structure.hs
-}

module Parser.Structure (module Parser.Structure) where

import Control.Applicative (Alternative ((<|>)))
import Parser.Type (Parser(..))
import Ast.Type(Structure(..), Type(..), Operable(..), Ast(..), Operation (CallStd))
import Parser.Symbol(parseType, parseSymbol)
import Parser.Operable(parseDefinitionName, parseOperable, parseOpOperation, parseElement)
import Parser.Syntax(parseWithSpace, parseMany, parseManyValidOrEmpty, parseMaybeparenthesis)
import Parser.Char(parseChar, parseAChar, parseOpeningParenthesis, parseClosingParenthesis, parseOpeningCurlyBraquet, parseClosingCurlyBraquet)
import Parser.Error(replaceErr)
import Parser.Ast (parseAst)
import Parser.Operation (parseOperation, checkOperator)
import Eval.Operator (Operator (Add, Sub, Mul, Div, Mod))
import Eval.Atom (Atom(AtomI))

----------------------------------------------------------------

parseAstStructure :: Parser Ast
parseAstStructure = AstStructure <$> (
        parseVarDefinition
    <|> parseVarAssignation
    <|> parseReturn
    <|> parseIf
    <|> parseWhile
    -- <|> parseFor
    -- <|> parseSingle
    -- <|> parseBlock
    -- <|> parseSequence
    )

----------------------------------------------------------------

acceptableCharacters :: [Char]
acceptableCharacters = ['a'..'z']
                    ++ ['A'..'Z']
                    ++ ['0'..'9']
                    ++ ['|', '/', '[', ']', '(', ')', '{', '}', '-', '_', '"', '\'']
                    ++ [' ', '+', '?', '.', ':', '!', ';', '\\']

parseStringWithHandleBackslash :: Parser String
parseStringWithHandleBackslash =
  replaceErr "Syntaxe error: bad return"
  (parseMany (((parseChar '\\') *> (parseChar '\\')) <|> ((parseChar '\\') *> parseAChar) <|> parseAChar))

----------------------------------------------------------------

createVarDef :: Parser Type -> Parser String -> Parser (Maybe Operable) -> Parser Structure
createVarDef  parType parStr op = Parser $ \s p -> case runParser parType s p of
  Right(typ, str, pos) -> case runParser parStr str pos of
    Right(name, string, position) -> case runParser op string position of
        Right(ope, new_str, new_pos) -> Right((VarDefinition name typ ope), new_str, new_pos)
        Left a -> Left a
    Left a -> Left a
  Left a -> Left a

parseVarDefinition :: Parser Structure
parseVarDefinition =
  replaceErr "Syntaxe error: bad variable definition"
  ((createVarDef (parseType <* parseChar ' ') (parseWithSpace parseDefinitionName) (Just <$> (parseWithSpace (parseChar '=') *> parseOperable) <|> pure Nothing)) <* parseChar ';')

----------------------------------------------------------------

getIncrement :: String -> Maybe Operator
getIncrement "++" = Just Add
getIncrement "--" = Just Sub
getIncrement _ = Nothing

parseIncrement :: Parser String
parseIncrement =  parseSymbol "++"
              <|> parseSymbol "--"

parseIncrementOp :: String -> Parser Operable
parseIncrementOp name = Parser $ \s p -> case runParser (parseWithSpace $ checkOperator parseIncrement getIncrement) s p of
  Right (operand, newstr, newpos) -> Right (OpOperation $ CallStd operand [OpVariable name, OpValue $ AtomI 1], newstr, newpos)
  Left a -> Left a

getOpequality :: String -> Maybe Operator
getOpequality "+=" = Just Add
getOpequality "-=" = Just Sub
getOpequality "*=" = Just Mul
getOpequality "/=" = Just Div
getOpequality "%=" = Just Mod
getOpequality _ = Nothing

parseOpEquality :: Parser String
parseOpEquality = parseSymbol "+="
              <|> parseSymbol "-="
              <|> parseSymbol "*="
              <|> parseSymbol "/="
              <|> parseSymbol "%="

parseEqualityOp :: String -> Parser Operable
parseEqualityOp name = Parser $ \s p -> case runParser (parseWithSpace $ checkOperator parseOpEquality getOpequality) s p of
  Right (operand, newstr, newpos) -> case runParser (parseMaybeparenthesis parseElement) newstr newpos of
    Right (elemright, rstr, rpos) -> Right (OpOperation $ CallStd operand [OpVariable name, elemright], rstr, rpos)
    Left a -> Left a
  Left a -> Left a

parseVarEquality :: Parser Structure
parseVarEquality =
  replaceErr "Syntaxe error: bad assignment"
  (VarAssignation <$> parseWithSpace parseDefinitionName <*> (parseWithSpace (parseChar '=') *> parseElement <* parseWithSpace (parseChar ';')))

parseVarIncrementation :: Parser Structure
parseVarIncrementation = Parser $ \s p -> case runParser (parseWithSpace parseDefinitionName) s p of
  Right(name, newstr, newpos) -> case runParser (parseWithSpace (parseIncrementOp name) <* parseWithSpace (parseChar ';')) newstr newpos of
    Right (op, opstr, oppos) -> Right (VarAssignation name op, opstr, oppos)
    Left a -> Left a
  Left a -> Left a

parseVarOperation :: Parser Structure
parseVarOperation = Parser $ \s p -> case runParser (parseWithSpace parseDefinitionName) s p of
  Right(name, newstr, newpos) -> case runParser (parseWithSpace (parseEqualityOp name) <* parseWithSpace (parseChar ';')) newstr newpos of
    Right (op, opstr, oppos) -> Right (VarAssignation name op, opstr, oppos)
    Left a -> Left a
  Left a -> Left a

parseVarAssignation :: Parser Structure
parseVarAssignation = parseVarEquality
                  <|> parseVarIncrementation
                  <|> parseVarOperation

----------------------------------------------------------------

parseReturnWithParenthesis :: Parser Operable
parseReturnWithParenthesis = parseWithSpace (parseSymbol "return") *> parseOpeningParenthesis *> parseOperable <* parseWithSpace parseClosingParenthesis

parseReturnWithoutParenthesis :: Parser Operable
parseReturnWithoutParenthesis = parseWithSpace (parseSymbol "return") *> parseOperable

parseReturn :: Parser Structure
parseReturn =
  replaceErr "Syntaxe error: bad return"
  (Return <$> ((parseReturnWithParenthesis <|> parseReturnWithoutParenthesis) <* parseChar ';'))

----------------------------------------------------------------

parsecond :: Parser Operable
parsecond = parseWithSpace parseOpeningParenthesis *>
            parseWithSpace parseElement <*
            parseWithSpace parseClosingParenthesis

parseIf :: Parser Structure
parseIf = Parser $ \s p -> case runParser (parseWithSpace $ parseSymbol "if" *> parsecond) s p of
  Right (cond, condstr, condpos) -> case runParser parseThen condstr condpos of
    Right (thn, thnstr, thnpos) -> case runParser (parseMany parseElIf) thnstr thnpos of
      Right (elifs, elifstr, elifpos) -> case runParser parseElse elifstr elifpos of
        Right (els, elstr, elspos) -> Right (If ((cond, thn) : elifs) els, elstr, elspos)
        Left a -> Left a
      Left a -> Left a
    Left a -> Left a
  Left a -> Left a

parseThen :: Parser Ast
parseThen = parseWithSpace parseOpeningCurlyBraquet *>
            (AstStructure <$> parseWithSpace parseSequence) <*
            parseWithSpace parseClosingCurlyBraquet

parseElIf :: Parser (Operable, Ast)
parseElIf = Parser $ \s p -> case runParser (parseWithSpace $ parseSymbol "elif" *> parsecond) s p of
  Right (cond, condstr, condpos) -> case runParser parseThen condstr condpos of
    Right (thn, thnstr, thnpos) -> Right ((cond, thn), thnstr, thnpos)
    Left a -> Left a
  Left a -> Left a

parseElse :: Parser (Maybe Ast)
parseElse = Parser $ \s p -> case runParser (parseWithSpace $ parseSymbol "else") s p of
  Right (_, newstr, newpos) -> case runParser parseThen newstr newpos of
    Right (thn, thnstr, thnpos) -> Right (Just thn, thnstr, thnpos)
    Left a -> Left a
  Left _ -> Right (Nothing, s, p)

----------------------------------------------------------------

parseWhile :: Parser Structure
parseWhile = Parser $ \s p -> case runParser (parseWithSpace $ parseSymbol "while" *> parsecond) s p of
  Right (cond, condstr, condpos) -> case runParser parseThen condstr condpos of
    Right (thn, thnstr, thnpos) -> Right (While cond thn, thnstr, thnpos)
    Left a -> Left a
  Left a -> Left a

parseFor :: Parser Structure
parseFor = Parser $ \s p -> case runParser (parseWithSpace $ parseSymbol "for" *> parseDefinitionName) s p of
  Right (it, itstr, itpos) -> case runParser (parseWithSpace $ parseSymbol "in" *> parseOperable) itstr itpos of
    Right (op, opstr, oppos) -> case runParser parseThen opstr oppos of
      Right (thn, thnstr, thnpos) -> Right (For it op thn, thnstr, thnpos)
      Left a -> Left a
    Left a -> Left a
  Left a -> Left a

----------------------------------------------------------------

parseSingle :: Parser Structure
parseSingle =  Single <$> parseAst

parseSequence :: Parser Structure
parseSequence = Sequence <$> parseMany parseAst

----------------------------------------------------------------


