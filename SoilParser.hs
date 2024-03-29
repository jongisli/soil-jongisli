--
-- Skeleton for Soil parser
-- To be used at the exam for Advanced Programming, B1-2012
--

module SoilParser where

import SimpleParse
import SoilAst
import Data.Char(isSpace,isLower,isUpper,isDigit,isAlpha,isAlphaNum)

import System.IO
import Control.Exception.Base

type Error = String

keyWords = ["self", "to", "create", "with", "become", 
            "let", "from", "case", "send", "concat",
            "if", "then", "else", "end"]

digit :: Parser Char
digit = satisfy isDigit

letter :: Parser Char
letter = satisfy isAlpha 

nameRest :: Parser Char
nameRest = satisfy isAlpha <|> satisfy isDigit <|> satisfy (\x -> x == '_')

name :: Parser Name
name = token $ do c <- letter
                  cs <- many nameRest
                  if (c:cs `elem` keyWords)
                     then reject
                     else return (c:cs)

ident :: Parser Ident
ident = do schar '#'
           n <- name
           return n

-- A Prim parser that doesn't allow concatenation
-- used in the Prim parser below to avoid loops when
-- concatenating.
prim' :: Parser Prim
prim' = self <|> name' <|> ident'
       where self = do symbol "self"
                       return Self
             ident' = do i <- ident
                         return (Id i)
             name' = do n <- name
                        return (Par n)


prim :: Parser Prim
prim = self <|> name' <|> ident' <|> concat
       where self = do symbol "self"
                       return Self
             ident' = do i <- ident
                         return (Id i)
             name' = do n <- name
                        return (Par n)
             concat = chainl1 prim' (symbol "concat" >> return Concat)

someArgs :: Parser [Prim]
someArgs = do p  <- prim `sepBy1` (schar ',')
              return p

args :: Parser [Prim]
args = someArgs' <|> emptyList
       where someArgs' = do a <- someArgs
                            return a
             emptyList = return []

fcall :: Parser (Prim, [Prim])
fcall = do p <- prim
           schar '('
           a <- args
           schar ')'
           return (p,a)

actop :: Parser ActOp
actop = sendTo <|> create <|> become
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
actops = many actop 

someparams :: Parser [Name]
someparams = do s <- name `sepBy` (schar ',')
                return s

parameters :: Parser [Name]
parameters = someparams


expr :: Parser Expr
expr =  case' <|> ifStm <|> acts
        where case' = do symbol "case"
                         p <- prim
                         symbol "of"
                         (c,e) <- cases
                         symbol "end"
                         return (CaseOf p c e) 
              ifStm = do symbol "if"
                         p1 <- prim
                         symbol "=="
                         p2 <- prim
                         symbol "then"
                         e1 <- expr
                         symbol "else"
                         e2 <- expr
                         symbol "end"
                         return (IfEq p1 p2 e1 e2)
              acts = do a <- actops
                        return (Acts a)
              

cases :: Parser ([([Name], Expr)], Expr)
cases = parCase <|> restCase
        where parCase = do schar '('
                           n <- parameters
                           schar ')'
                           schar ':'
                           e1 <- expr
                           (_,e2) <- cases
                           return ([(n,e1)], e2) 
              restCase = do schar '_'
                            schar ':'
                            e <- expr
                            return ([], e)     


fundef :: Parser Func
fundef = do symbol "let"
            i <- ident
            schar '('
            p <- parameters
            schar ')'
            symbol "from"
            n <- name
            schar '='
            e <- expr
            symbol "end"
            return (Func i p n e)


defops :: Parser ([Func], [ActOp])
defops = do f <- many fundef
            a <- actops
            return (f,a)


program :: Parser Program
program = defops
                           

parseString :: String -> Either Error Program
parseString s = case result of
                     [] -> Left "ERROR"
                     val -> Right (head $ val)
                where result = parse' program s


parseFile :: FilePath -> IO (Either Error Program)
parseFile f = do  
    handle <- openFile f ReadMode  
    contents <- hGetContents handle
    return (parseString contents)
    

