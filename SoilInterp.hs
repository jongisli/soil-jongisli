--
-- Skeleton for Soil interpreter
-- To be used at the exam for Advanced Programming, B1-2012
--

module SoilInterp where

import SoilAst
import SoilParser
import SimpleParse

import qualified Data.Map as M
import Data.Maybe

--
-- Part 1: Define a name environment
--
data NameEnv = NameEnv {mapping :: (M.Map Name Ident)}
             deriving(Show)

-- Functions for insert and lookup

nameInsert :: Name -> Ident -> NameEnv -> Either Error NameEnv
nameInsert n i env = if ismember
                        then Left "Name already exists"
                        else Right (NameEnv (M.insert n i (mapping env)))
                     where ismember = M.member n (mapping env)

nameLookup :: Name -> NameEnv -> Either Error Ident
nameLookup n env = case look of
                    Nothing -> Left "Name does not exist"
                    val     -> Right (fromMaybe "" val)
               where look = M.lookup n (mapping env)

                      
lst = [("lol","#rofl"), ("jon","#gisli")]
mapz = NameEnv {mapping = M.fromList lst}

--
-- Part 2: Define a function environment
--
data FuncEnv = FuncEnv {mappingf :: (M.Map Ident Func)}
             deriving(Show)

-- Functions for insert and lookup

funcInsert :: Ident -> Func -> FuncEnv -> Either Error FuncEnv
funcInsert i f env = if ismember
                        then Left "Function with that identity already exists"
                        else Right (FuncEnv (M.insert i f (mappingf env)))
                     where ismember = M.member i (mappingf env)

funcLookup :: Ident -> FuncEnv -> Either Error Func
funcLookup i env = case look of
                    Nothing -> Left "Function with that identity does not exist"
                    Just val -> Right val 
               where look = M.lookup i (mappingf env)

fun1 = head $ parse' fundef "let #printer () from message = send (message) to #println end"
fun2 = head $ parse' fundef  "let #lolo(jon,gisli) from message = send (message,jon,gisli) to #println end"
lzt = [("#gamli", fun1),("#sosa", fun2)]
mappo = FuncEnv {mappingf = M.fromList lzt}

--
-- Part 3: Degine a process and process queue
--
type Message = [Ident]

data Process = Process { function  :: Ident
                       , arguments :: [Ident]
                       , mailbox   :: [Message]}

data ProcessQueue = String

-- Function for process modification

-- --
-- -- Part 4: Define and implement a process step
-- --
-- processStep :: ...

-- --
-- -- Part 5: Define and implement the roind-robin algorithm
-- --
-- nextProcessRR :: ...

-- --
-- -- Part 6: Implement the round-robin evaluator
-- --
-- runProgRR :: Int -> Program -> ([String], [String])

-- --
-- -- Part 7: Implement a find all possible executions evaluator
-- --
-- nextProcAll :: ...

-- runProgAll :: Int -> Program -> [([String], [String])]