module Parser
     ( getKripke
     , getLTL
     ) where

import Text.ParserCombinators.Parsec
import qualified MySet as S
import Type
import LTL
import Kripke


getKripke :: String -> IO (Kripke)
getKripke str =
  case parse parseKripke "ERROR" str of
    Left err -> error "invalid Kripke structure: Kripke = (States,Initial,Relations,Labelings)\neg) ({1, 2}, 1, {(1, 1), (1, 2), (2, 2)}, {(1,{p,q}), (2,{})})"
    Right k -> return k

parseKripke :: Parser Kripke
parseKripke = Kripke <$> (spaces *> char '(' *> parseStates) <*> (char ',' *> parseInitial) <*> (char ',' *> parseRelations) <*> (char ',' *> parseLabeling <* char ')' <* spaces)

parseStates :: Parser (States String)
parseStates =
  spaces *> char '{' *> state `sepBy` (char ',') <* char '}' <* spaces >>=
  return . S.fromList

parseInitial :: Parser (String)
parseInitial = state

parseRelations :: Parser (S.Set (String, String))
parseRelations =
  spaces *> char '{' *> relation `sepBy` (char ',') <* char '}' <* spaces >>=
  return . S.fromList

parseLabeling  :: Parser (S.Set (String, S.Set String))
parseLabeling =
  spaces *> char '{' *> Parser.labeling `sepBy` (char ',') <* char '}' <* spaces >>=
  return . S.fromList

state :: Parser String
state = spaces *> many1 (noneOf " (),{}") <* spaces

relation :: Parser (String, String)
relation = (,) <$> (spaces *> char '(' *> state) <*> (char ',' *> state <* char ')' <* spaces)

labeling :: Parser (String, S.Set String)
labeling = (,) <$> (spaces *> char '(' *> state) <*> (char ',' *> Parser.label <* char ')' <* spaces)

label :: Parser (S.Set String)
label = spaces *> char '{' *> (spaces *> many1 (noneOf " (),{}") <* spaces) `sepBy` char ',' <* char '}' <* spaces >>= return . S.fromList



getLTL :: String -> IO (LTL String)
getLTL str =
  case parse parseLTL "ERROR" str of
    Left err -> error $ "invalid LTL formula: you can use true, false, Ap, not, and, or, X, U, F, and G.\neg) F((true) U ((not (p)) and (q)))"
    Right ltl -> return ltl

parseLTL :: Parser (LTL String)
parseLTL = do
  choice [ try parseUntil
         , try parseAnd
         , try parseOr
         , try parseNext
         , try parseFuture
         , try parseGlobal
         , try parseNot
         , try parseTop
         , try parseBottom
         , try parseAp
         ]

parseTop :: Parser (LTL a)
parseTop = spaces *> string "true" *> spaces *> return Top

parseBottom :: Parser (LTL a)
parseBottom = spaces *> string "false" *> spaces *> return Bottom

parseAp :: Parser (LTL String)
parseAp = spaces *> many1 (noneOf " ()") <* spaces >>= return . LAP

parseNot :: Parser (LTL String)
parseNot = spaces *> string "not" *> spaces *> char '(' *> parseLTL <* spaces <* char ')' <* spaces >>= return . Not

parseAnd :: Parser (LTL String)
parseAnd = LAnd <$> (spaces *> char '(' *> parseLTL <* char ')' <* spaces) <*> (string "and" *> spaces *> char '(' *> parseLTL <* char ')' <* spaces)

parseOr :: Parser (LTL String)
parseOr = LOr <$> (spaces *> char '(' *> parseLTL <* char ')' <* spaces) <*> (string "or" *> spaces *> char '(' *> parseLTL <* char ')' <* spaces)

parseNext :: Parser (LTL String)
parseNext = spaces *> char 'X' *> spaces *> char '(' *> parseLTL <* char ')' <* spaces >>= return . X

parseUntil :: Parser (LTL String)
parseUntil= U <$> (spaces *> char '(' *> parseLTL <* char ')' <* spaces) <*> (char 'U' *> spaces *> char '(' *> parseLTL <* char ')' <* spaces)

parseFuture :: Parser (LTL String)
parseFuture = spaces *> char 'F' *> spaces *> char '(' *> parseLTL <* char ')' <* spaces >>= return . future

parseGlobal :: Parser (LTL String)
parseGlobal = spaces *> char 'G' *> spaces *> char '(' *> parseLTL <* char ')' <* spaces >>= return . global
