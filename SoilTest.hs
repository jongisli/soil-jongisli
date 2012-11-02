import Test.HUnit
import SoilParser
import SoilAst
import SoilInterp
import SimpleParse

import qualified Data.Map as M

helloWorldString = "let #print() from message = \
\  send (message) to #println\
\end\

\create #hw1 with #print()\
\create #hw2 with #print()\
\send (#Hello) to #hw1\
\send (#World) to #hw2"

helloWorldAst = ([Func {funcname = "print", params = [], receive = "message", body = Acts [(SendTo [Par "message"] (Id "println"))]}],[Create (Id "hw1") (Id "print") [] ,Create (Id "hw2") (Id "print") [] ,SendTo [Id "Hello"] (Id "hw1") ,SendTo [Id "World"] (Id "hw2")])

helloWorldAst' = parseString helloWorldString

parseHelloWorldTest = TestCase $ assertBool "Testing parsing helloWorld.soil" $
                      Right helloWorldAst == helloWorldAst'

cleanUpString = "let #dub() from message  = \
\  case message of\
\    (sender, msg) : send (self, msg) to sender\
\                    send (self, msg) to sender\
\    _             : send (#FaultyMessage) to #println \
\  end\
\end\

\let #half(state) from message = \
\  if state == #skip then\
\    become #half(#return)\
\    send (#SkippingMessage) to #println\
\  else\
\    case message of\
\      (sender, msg) : become #half(#skip)\
\                      send (self, msg) to sender\
\      _             : send (#FaultyMessage) to #println \
\    end\
\  end\
\end\
    
\create #dubproc with #dub()\
\create #halfproc with #half(#return)\
\send (#halfproc, #foo) to #dubproc"

cleanUpAst = ([Func {funcname = "dub", params = [], receive = "message", 		body = CaseOf (Par "message") [			(["sender", "msg"]				,Acts [					(SendTo [Self, Par "msg"] (Par "sender")),					(SendTo [Self, Par "msg"] (Par "sender"))]			)]			(Acts [(SendTo [Id "FaultyMessage"] (Id "println"))])		}	,Func {		funcname = "half", 		params = ["state"], 		receive = "message", 		body = IfEq (Par "state") (Id "skip")			(Acts [					(Become (Id "half") [Id "return"]),					(SendTo [Id "SkippingMessage"] (Id "println"))]			)			(CaseOf (Par "message") [				(["sender", "msg"]					,Acts [						(Become (Id "half") [Id "skip"]),						(SendTo [Self, Par "msg"] (Par "sender"))]				)]				(Acts [(SendTo [Id "FaultyMessage"] (Id "println"))])			)		}	],	[Create (Id "dubproc") (Id "dub") []	,Create (Id "halfproc") (Id "half") [Id "return"]	,SendTo [Id "halfproc", Id "foo"] (Id "dubproc")	]) 

cleanUpAst' = parseString cleanUpString

parseCleanUpTest = TestCase $ assertBool "Testing parsing cleanUp.soil" $
                   Right cleanUpAst == cleanUpAst'

lst = [("lol","#rofl"), ("jon","#gisli")]
lstAdd = [("lol","#rofl"), ("jon","#gisli"), ("bobo", "#hobo")]

mapz = NameEnv {mapping = M.fromList lst}
mapzAdd = NameEnv {mapping = M.fromList lstAdd}

nameInsertTest = TestCase $ assertBool "Testing NameEnv inserting" $
                 Right mapzAdd == nameInsert "bobo" "#hobo" mapz

nameLookupTest = TestCase $ assertBool "Testing NameEnv lookup" $
                 Right "#rofl" == nameLookup "lol" mapz


fun1 = head $ parse' fundef "let #printer () from message = send (message) to #println end"
fun2 = head $ parse' fundef  "let #lolo(jon,gisli) from message = send (message,jon,gisli) to #println end"

lzt = [("#gamli", fun1), ("#sosa", fun2)]
lztAdd = [("#gamli", fun1), ("#sosa", fun2), ("#hehe",fun2)]

mappo = FuncEnv {mappingf = M.fromList lzt}
mappoAdd = FuncEnv {mappingf = M.fromList lztAdd}

funcInsertTest = TestCase $ assertBool "Testing FuncEnv inserting" $
                 Right mappoAdd == funcInsert "#hehe" fun2 mappo

funcLookupTest = TestCase $ assertBool "Testing FuncEnv lookup" $
                 Right fun1 == funcLookup "#gamli" mappo


tests = TestList [parseHelloWorldTest, parseCleanUpTest, nameInsertTest,
                  funcInsertTest, nameLookupTest, funcLookupTest]

main = runTestTT tests