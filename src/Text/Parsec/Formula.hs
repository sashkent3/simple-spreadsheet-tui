{-# LANGUAGE ImportQualifiedPost #-}

module Text.Parsec.Formula where

import Control.Monad.Free (Free (Free, Pure), iter)
import Control.Monad.Identity (Identity)
import Data.Char (chr, ord)
import Data.Foldable (foldl')
import Data.Functor.Dialects (Add ((:+:), (:-:)), Div ((:/:)), Mul ((:*:)), Sign (Neg, Pos))
import Data.Functor.Sum (Sum (InL, InR))
import Data.Spreadsheet (Formula, SpreadsheetIndex)
import Interpreters (AllDialect, AllResult, (:|:))
import Text.Parsec (Parsec, optionMaybe)
import Text.Parsec qualified as P
import Text.Parsec.Expr (Assoc (AssocLeft), Operator (Infix, Postfix, Prefix), OperatorTable, buildExpressionParser)
import Text.Parsec.Language (haskell)
import Text.Parsec.Token (decimal, naturalOrFloat, parens)

type Value = Double

data Op = ONeverParen | OAdd | OSub | OMul | ODiv deriving (Eq)

base26Digits :: [Char]
base26Digits = ['A' .. 'Z']

base26DigitToInt :: Char -> Int
base26DigitToInt c = ord c - ord 'A' + 1

intToBase26Digit :: Int -> Char
intToBase26Digit i = chr $ ord 'A' + i - 1

nonZeroRemainder :: Integral a => a -> a -> a
nonZeroRemainder x y = case x `mod` y of 0 -> y; n -> n

intToBase26 :: Int -> String
intToBase26 i = go (i + 1) ""
  where
    go 0 = id
    go i = go ((i - rem) `div` 26) . (intToBase26Digit rem :)
      where
        rem = i `nonZeroRemainder` 26

spreadsheetIndexParser :: Parsec String u SpreadsheetIndex
spreadsheetIndexParser = do
  col <- foldl' (\a i -> a * 26 + base26DigitToInt i) 0 <$> P.many1 (P.oneOf base26Digits)
  row <- fromInteger <$> decimal haskell
  return (row, col - 1)

valueParser :: Parsec String u Value
valueParser = do
  v <- naturalOrFloat haskell
  case v of
    Left i -> return $ fromInteger i
    Right d -> return d

termParser :: Parsec String u (Formula AllDialect Value)
termParser = optionMaybe spreadsheetIndexParser >>= maybe (Pure . Right <$> valueParser) (return . posFormula . Pure . Left)

posFormula :: Formula AllDialect a -> Formula AllDialect a
posFormula = Free . InL . Pos

negFormula :: Formula AllDialect a -> Formula AllDialect a
negFormula = Free . InL . Neg

addFormula :: Formula AllDialect a -> Formula AllDialect a -> Formula AllDialect a
addFormula a b = Free . InR . InL $ (a :+: b)

subFormula :: Formula AllDialect a -> Formula AllDialect a -> Formula AllDialect a
subFormula a b = Free . InR . InL $ (a :-: b)

mulFormula :: Formula AllDialect a -> Formula AllDialect a -> Formula AllDialect a
mulFormula a b = Free . InR . InR . InL $ (a :*: b)

divFormula :: Formula AllDialect a -> Formula AllDialect a -> Formula AllDialect a
divFormula a b = Free . InR . InR . InR $ (a :/: b)

operatorTable :: [[Operator String u Identity (Formula AllDialect Value)]]
operatorTable =
  [ [Prefix (P.char '-' >> return negFormula), Prefix (P.char '+' >> return id)],
    [Infix (P.char '*' >> return mulFormula) AssocLeft, Infix (P.char '/' >> return divFormula) AssocLeft],
    [Infix (P.char '+' >> return addFormula) AssocLeft, Infix (P.char '-' >> return subFormula) AssocLeft]
  ]

formulaParser :: Parsec String u (Formula AllDialect Value)
formulaParser =
  buildExpressionParser operatorTable $
    P.between P.spaces P.spaces (parens haskell formulaParser P.<|> termParser)

parseFormula :: String -> Either P.ParseError (Formula AllDialect Value)
parseFormula = P.parse (formulaParser <* P.eof) ""

showFormula :: Formula AllDialect Value -> String
showFormula = fst . iter showOp . fmap showTerm
  where
    showTerm (Left (row, col)) = (intToBase26 col ++ show row, ONeverParen)
    showTerm (Right v) = (show v, ONeverParen)

    parens s = "(" ++ s ++ ")"

    showOp (InL (Pos (a, _))) = (a, ONeverParen)
    showOp (InL (Neg (a, _))) = ('-' : a, ONeverParen)
    showOp (InR (InL ((a, _) :+: (b, _)))) = (a ++ " + " ++ b, OAdd)
    showOp (InR (InL ((a, _) :-: (b, bOp)))) =
      let bPar = case bOp of
            OAdd -> parens b
            OSub -> parens b
            _ -> b
       in (a ++ " - " ++ bPar, OSub)
    showOp (InR (InR (InL ((a, aOp) :*: (b, bOp))))) =
      let aPar = case aOp of
            OAdd -> parens a
            OSub -> parens a
            _ -> a
          bPar = case bOp of
            OAdd -> parens b
            OSub -> parens b
            _ -> b
       in (aPar ++ " * " ++ bPar, OMul)
    showOp (InR (InR (InR ((a, aOp) :/: (b, bOp))))) =
      let aPar = case aOp of
            OAdd -> parens a
            OSub -> parens a
            _ -> a
          bPar = case bOp of
            ONeverParen -> b
            _ -> parens b
       in (aPar ++ " / " ++ bPar, ODiv)
