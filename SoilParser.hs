--
-- Skeleton for Soil parser
-- To be used at the exam for Advanced Programming, B1-2012
--

module SoilParser where

import SimpleParse
import SoilAst
import Data.Char(isSpace,isLower,isUpper,isDigit,isAlpha,isAlphaNum)

digit :: Parser Char
digit = satisfy isDigit

letter :: Parser Char
letter = satisfy isAlpha 

nameRest :: Parser Char
nameRest = satisfy isAlpha <|> satisfy isDigit <|> satisfy (\x -> x == '_')

name :: Parser Name
name = token $ do c <- letter -- token might let it hang
                  cs <- many nameRest
                  return (c:cs)

ident :: Parser Ident
ident = token $ do c <- satisfy (\x -> x == '#') -- token might let it hang
                   cs <- name
                   return cs

prim :: Parser Prim
prim = token $ self <|> name' <|> ident'
       where self = do symbol "self"
                       return Self
             ident' = do i <- ident
                         return (Id i)
             name' = do n <- name
                        return (Par n)
            

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





parseString :: String -> Either Error Program
parseString s = Left "Error bro"

testTxt = "send (#ok) to self"
-- Right ([], [SendTo [Id "ok"] Self])

--parseFile :: FilePath -> IO (Either Error Program)