import Test.HUnit
import SoilParser
import SoilAst

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

tests = TestList [parseHelloWorldTest, parseCleanUpTest]

main = runTestTT tests