{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}

module Language.LARS.Parser
    ( Parser
    , program
    , rule
    , window
    , atom
    , sc
    ) where

import Control.Applicative
import Control.Monad.Combinators.Expr as E
import Data.Text (Text, pack)
import Data.Expr
import Data.Void
import Language.LARS.AST
import Text.Megaparsec ((<?>))

import qualified Control.Applicative.Combinators as C
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L

import Prelude hiding (head)

type Parser = P.Parsec Void Text

sc :: Parser ()
sc = L.space P.space1 lineCmnt blockCmnt
  where
    lineCmnt = L.skipLineComment "//"
    blockCmnt = L.skipBlockComment "/*" "*/"

rule :: Parser Rule
rule =
    Rule <$> (head <* sc) <*>
    C.option [] (sc *> symbol ":-" *> (sc *> bodyElement) `P.sepBy1` P.char ',') <*
    P.char '.'

head :: Parser Head
head = Head <$> (P.try atAtom <|> (Atom <$> atom))

bodyElement :: Parser BodyElement
bodyElement =
    (BodyAtom <$> (P.try wAtom <|> (Atom <$> atom))) <|>
    (BodyRelation <$> relation)

number :: (Num a, RealFloat a) => Parser a
number = fromIntegral <$> (L.decimal :: Parser Int) <|> L.float

value :: Parser Value
value = varg <|> carg <|> strg <|> aqut
    where
    varg = Variable <$> variable
    carg = Constant <$> number
    strg = String <$> quoted (P.takeWhileP Nothing (/= '"'))

aqut :: Parser Value
aqut = Antiquoted <$> (P.char '$' *> C.some P.letterChar)

notIn :: Parser Bool
notIn = C.option True (C.choice [False <$ symbol "not in", True <$ symbol "in"])

atAtom :: Parser Atom
atAtom = AtAtom <$> atom <*> (sc *> symbol "at" *> sc *> value) <?> "timed atom"

atom :: Parser BasicAtom
atom =
    BasicAtom <$> C.option True (False <$ (sc *> symbol "not" <* sc)) <*>
    predicate <*>
    C.option [] (parens (value `P.sepBy` P.char ',') <?> "simple atom")

diamWAtom :: Parser Atom
diamWAtom =
    HappenedAtom <$> atom <*> (sc *> notIn) <*>
    (sc *> window) <?> "diamond atom"

boxWAtom :: Parser Atom
boxWAtom =
    AlwaysAtom <$> (atom <* sc <* symbol "always") <*> (sc *> notIn) <*>
    (sc *> window) <?> "box atom"

atWAtom :: Parser Atom
atWAtom =
    AtWAtom <$> atom <*> (sc *> symbol "at" *> sc *> value) <*> (sc *> notIn) <*>
    (sc *> window) <?> "at atom"

wAtom :: Parser Atom
wAtom = C.choice [P.try atWAtom, P.try boxWAtom, P.try diamWAtom]

predicate :: Parser Text
predicate =
    pack <$>
    ((:) <$> (P.char '-' <|> P.lowerChar) <*> C.many P.alphaNumChar) <?>
    "predicate identifier"

variable :: Parser Text
variable =
    pack <$>
    ((:) <$> P.upperChar <*> C.many (P.alphaNumChar <|> P.char '\'')) <?>
    "variable name"

signature :: Parser Signature
signature =
    Signature <$> predicate <*> (P.char '/' *> L.decimal) <?> "signature"

window :: Parser Window
window = brackets wspec
  where
    wspec = do
        n <- (Constant . fromIntegral @Int <$> L.decimal) <|> aqut
        _ <- sc
        f <- C.choice
                [ SlidingTimeWindow Sec <$ (symbol "sec" <|> symbol "s")
                , SlidingTimeWindow Millisec <$ symbol "msec"
                , SlidingTimeWindow Minute <$ symbol "min"
                , SlidingTimeWindow Hour <$ symbol "h"
                ]
        pure $ f n

relation :: Parser Relation
relation = do
    a <- arith
    sc
    r <-
        C.choice
            [ RelEq <$ symbol "="
            , RelNeq <$ symbol "!="
            , RelGEq <$ symbol ">="
            , RelLEq <$ symbol "<="
            , RelGt <$ symbol ">"
            , RelLt <$ symbol "<"
            ]
    b <- arith
    pure $ r a b

arith :: Parser Arith
arith = E.makeExprParser term table <?> "arithmetic expression"
  where
    term = parens arith <|> (ArithValue <$> (value <* sc)) <?> "term"
    binary n f = E.InfixL (f <$ symbol n)
    table =
        [ [binary "^" Exp]
        , [binary "%" Modulo, binary "/" Divide, binary "*" Multiply]
        , [binary "-" Subtract, binary "+" Add]
        ]

parens :: Parser a -> Parser a
parens = C.between (P.char '(') (P.char ')')

brackets :: Parser a -> Parser a
brackets = C.between (P.char '[') (P.char ']')

program :: Parser Program
program =
    C.many $
    P.choice
        [ StmtRule <$> (sc *> rule <* C.optional (P.newline <* sc))
        , StmtShow <$>
          (sc *> P.string "#show" *> sc *> signature <* P.char '.' <*
           C.optional (P.newline <* sc))
        ]

symbol :: Text -> Parser Text
symbol = L.symbol sc

quoted :: Parser c -> Parser c
quoted = P.between (P.char '"') (P.char '"')
