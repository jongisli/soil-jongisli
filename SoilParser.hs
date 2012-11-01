--
-- Skeleton for Soil parser
-- To be used at the exam for Advanced Programming, B1-2012
--

module SoilParser where

import SimpleParse
import SoilAst
import Data.Char(isSpace,isLower,isUpper,isDigit,isAlpha,isAlphaNum)

keyWords = ["self", "to", "create", "with", "become", 
            "let", "from", "case", "send", "concat",
            "if", "then", "else"]

digit :: Parser Char
digit = satisfy isDigit

letter :: Parser Char
letter = satisfy isAlpha 

nameRest :: Parser Char
nameRest = satisfy isAlpha <|> satisfy isDigit <|> satisfy (\x -> x == '_')

name :: Parser Name
name = do c <- letter
          cs <- many nameRest
          if (c:cs `elem` keyWords)
             then reject
             else return (c:cs)

ident :: Parser Ident
ident = do c <- satisfy (\x -> x == '#')
           cs <- name
           return cs

-- A Prim parser that doesn't allow concatenation
-- used in the Prim parser below to avoid loops when
-- concatenating.
prim' :: Parser Prim
prim' = token $ self <|> name' <|> ident'
       where self = do symbol "self"
                       return Self
             ident' = do i <- ident
                         return (Id i)
             name' = do n <- name
                        return (Par n)

prim :: Parser Prim
prim = token $ self <|> name' <|> ident' <|> concat
       where self = do symbol "self"
                       return Self
             ident' = do i <- ident
                         return (Id i)
             name' = do n <- name
                        return (Par n)
             concat = chainl1 prim' (symbol "concat" >> return Concat)
             -- expr = chainl1 prim (char ’+’ >> return Add)

someArgs :: Parser [Prim]
someArgs = token $ do p  <- prim `sepBy1` (schar ',')
                      return p

args :: Parser [Prim]
args = token $ someArgs' <|> emptyList
       where someArgs' = do a <- someArgs
                            return a
             emptyList = return []

fcall :: Parser (Prim, [Prim])
fcall = token $ do p <- prim
                   schar '('
                   a <- args
                   schar ')'
                   return (p,a)

actop :: Parser ActOp
actop = token $ sendTo <|> create <|> become
        where sendTo = do symbol "send"
                          schar '('
                          a <- args
                          schar ')'
                          symbol "to"
                          p <- prim
                          return (SendTo a p)
              create = do symbol "create"
                          p <- prim
                          symbol "with"
                          (a,as) <- fcall
                          return (Create p a as)
              become = do symbol "become"
                          (a,as) <- fcall
                          return (Become a as)

actops :: Parser [ActOp]
actops = token $ many actop





parseString :: String -> Either Error Program
parseString s = Left "Error bro"

-- actop test
testTxt1 = "send (#ok) to self"
-- actops test
testTxt2 = "send (#ok) to self create jon with gisli(yo,yo)"
-- keyword test: should fail
testTxt3 = "send (#create) to self"

-- Right ([], [SendTo [Id "ok"] Self])

--parseFile :: FilePath -> IO (Either Error Program)