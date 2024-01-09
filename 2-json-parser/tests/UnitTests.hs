module Tests where

import Tokenizer 
import Parser
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain unitTests 

unitTests :: TestTree
unitTests = testGroup "unit tests" [tokenizerTests, parserTests]

parserTests :: TestTree 
parserTests = testGroup "parser tests" [ 
    testCase "parse simple json object" $
      parseObjectExpression (createTokenizer "{}") @?= Just (ObjectExpression { getFieldExpressions = [] }, []),
    testCase "parse simple json object with spaces" $
      parseObjectExpression (createTokenizer "{  }") @?= Just (ObjectExpression { getFieldExpressions = [] }, []),
    testCase "json object with string field" $ 
      parseObjectExpression (createTokenizer "{\"hello\" : \"world\" }") @?= Just (
            ObjectExpression { getFieldExpressions = [(StringExpression { getStringValue = "hello" }, StringValueExpression $ StringExpression { getStringValue = "world" })] }, []),
    testCase "json object with fields of different types" $ 
      parseObjectExpression (createTokenizer "{ \
                  \ \"key1\": true,       \
                  \ \"key2\": false,      \
                  \ \"key3\": null,       \
                  \ \"key4\": \"value\",  \
                  \ \"key5\": 101         \
                  \ }" ) @?= Just (
            ObjectExpression { getFieldExpressions = [
              (StringExpression { getStringValue = "key1" }, BooleanValueExpression { getBooleanValue = True }),
              (StringExpression { getStringValue = "key2" }, BooleanValueExpression { getBooleanValue = False }),
              (StringExpression { getStringValue = "key3" }, NullValueExpression ),
              (StringExpression { getStringValue = "key4" }, StringValueExpression $ StringExpression { getStringValue = "value" }),
              (StringExpression { getStringValue = "key5" }, NumberValueExpression { getNumberValue = 101 })
            ]}, [])
  ]

tokenizerTests :: TestTree 
tokenizerTests = testGroup "tokenizer tests" [ 
    testCase "empty string" $
      createTokenizer "  \
      \     \n\
      \ " @?= [
                    Token {getTokenType = Whitespace, getTokenValue = "       \n "} 
                  ],
    testCase "empty json object" $
      createTokenizer "{}" @?= [
                    Token {getTokenType = OpenBracket, getTokenValue = "{"}, 
                    Token {getTokenType = CloseBracket, getTokenValue = "}"}
                  ],
    testCase "json object with spaces" $ 
      createTokenizer "{  }" @?= [ 
                    Token {getTokenType = OpenBracket, getTokenValue = "{"}, 
                    Token {getTokenType = Whitespace, getTokenValue = "  "}, 
                    Token {getTokenType = CloseBracket, getTokenValue = "}"}
                  ],
    testCase "json object with string field" $ 
      createTokenizer "{\"hello\" : \"world\" }" @?= [ 
                    Token {getTokenType = OpenBracket, getTokenValue = "{"}, 
                    Token {getTokenType = StringType, getTokenValue = "\"hello\""}, 
                    Token {getTokenType = Whitespace, getTokenValue = " "}, 
                    Token {getTokenType = Colon, getTokenValue = ":"}, 
                    Token {getTokenType = Whitespace, getTokenValue = " "}, 
                    Token {getTokenType = StringType, getTokenValue = "\"world\""}, 
                    Token {getTokenType = Whitespace, getTokenValue = " "}, 
                    Token {getTokenType = CloseBracket, getTokenValue = "}"}
                  ],
    testCase "json object with fields of different types" $ 
      createTokenizer " {           \
            \  \"key1\": true,      \
            \  \"key2\": false,     \
            \  \"key3\": null,      \
            \  \"key4\": \"value\", \
            \  \"key5\": -100.1e+10 }" @?= [ 
                    Token {getTokenType = Whitespace, getTokenValue = " "}, 
                    Token {getTokenType = OpenBracket, getTokenValue = "{"},
                    Token {getTokenType = Whitespace, getTokenValue = "             "}, 

                    Token {getTokenType = StringType, getTokenValue = "\"key1\""}, 
                    Token {getTokenType = Colon, getTokenValue = ":"}, 
                    Token {getTokenType = Whitespace, getTokenValue = " "}, 
                    Token {getTokenType = BooleanType, getTokenValue = "true"}, 
                    Token {getTokenType = Comma, getTokenValue = ","}, 

                    Token {getTokenType = Whitespace, getTokenValue = "        "}, 
                    Token {getTokenType = StringType, getTokenValue = "\"key2\""}, 
                    Token {getTokenType = Colon, getTokenValue = ":"}, 
                    Token {getTokenType = Whitespace, getTokenValue = " "}, 
                    Token {getTokenType = BooleanType, getTokenValue = "false"}, 
                    Token {getTokenType = Comma, getTokenValue = ","}, 

                    Token {getTokenType = Whitespace, getTokenValue = "       "}, 
                    Token {getTokenType = StringType, getTokenValue = "\"key3\""}, 
                    Token {getTokenType = Colon, getTokenValue = ":"}, 
                    Token {getTokenType = Whitespace, getTokenValue = " "}, 
                    Token {getTokenType = NullType, getTokenValue = "null"}, 
                    Token {getTokenType = Comma, getTokenValue = ","}, 

                    Token {getTokenType = Whitespace, getTokenValue = "        "}, 
                    Token {getTokenType = StringType, getTokenValue = "\"key4\""}, 
                    Token {getTokenType = Colon, getTokenValue = ":"}, 
                    Token {getTokenType = Whitespace, getTokenValue = " "}, 
                    Token {getTokenType = StringType, getTokenValue = "\"value\""}, 
                    Token {getTokenType = Comma, getTokenValue = ","}, 

                    Token {getTokenType = Whitespace, getTokenValue = "   "}, 
                    Token {getTokenType = StringType, getTokenValue = "\"key5\""}, 
                    Token {getTokenType = Colon, getTokenValue = ":"}, 
                    Token {getTokenType = Whitespace, getTokenValue = " "}, 
                    Token {getTokenType = NumberType, getTokenValue = "-100.1e+10"}, 

                    Token {getTokenType = Whitespace, getTokenValue = " "}, 
                    Token {getTokenType = CloseBracket, getTokenValue = "}"}
                  ],
    testCase "json object with fields of complex types" $ 
      createTokenizer " {                           \
            \  \"key1\": \"value\",                 \
            \  \"key2\": 101,                       \
            \  \"key3\": { \"key5\": true },        \
            \  \"key4\": [                          \
            \     {}, {                             \
            \      \"key6\": false                  \
            \    }]                                 \
            \ }" @?= [ 
                    Token {getTokenType = Whitespace, getTokenValue = " "}, 
                    Token {getTokenType = OpenBracket, getTokenValue = "{"},
                    Token {getTokenType = Whitespace, getTokenValue = "                             "}, 

                    Token {getTokenType = StringType, getTokenValue = "\"key1\""}, 
                    Token {getTokenType = Colon, getTokenValue = ":"}, 
                    Token {getTokenType = Whitespace, getTokenValue = " "}, 
                    Token {getTokenType = StringType, getTokenValue = "\"value\""}, 
                    Token {getTokenType = Comma, getTokenValue = ","}, 

                    Token {getTokenType = Whitespace, getTokenValue = "                   "}, 
                    Token {getTokenType = StringType, getTokenValue = "\"key2\""}, 
                    Token {getTokenType = Colon, getTokenValue = ":"}, 
                    Token {getTokenType = Whitespace, getTokenValue = " "}, 
                    Token {getTokenType = NumberType, getTokenValue = "101"}, 
                    Token {getTokenType = Comma, getTokenValue = ","}, 

                    Token {getTokenType = Whitespace, getTokenValue = "                         "}, 
                    Token {getTokenType = StringType, getTokenValue = "\"key3\""}, 
                    Token {getTokenType = Colon, getTokenValue = ":"}, 
                    Token {getTokenType = Whitespace, getTokenValue = " "}, 
                    Token {getTokenType = OpenBracket, getTokenValue = "{"}, 
                    Token {getTokenType = Whitespace, getTokenValue = " "}, 
                    Token {getTokenType = StringType, getTokenValue = "\"key5\""}, 
                    Token {getTokenType = Colon, getTokenValue = ":"}, 
                    Token {getTokenType = Whitespace, getTokenValue = " "}, 
                    Token {getTokenType = BooleanType, getTokenValue = "true"}, 
                    Token {getTokenType = Whitespace, getTokenValue = " "}, 
                    Token {getTokenType = CloseBracket, getTokenValue = "}"}, 
                    Token {getTokenType = Comma, getTokenValue = ","}, 

                    Token {getTokenType = Whitespace, getTokenValue = "          "}, 
                    Token {getTokenType = StringType, getTokenValue = "\"key4\""}, 
                    Token {getTokenType = Colon, getTokenValue = ":"}, 
                    Token {getTokenType = Whitespace, getTokenValue = " "}, 
                    Token {getTokenType = OpenSquareBracket, getTokenValue = "["}, 
                    Token {getTokenType = Whitespace, getTokenValue = "                               "}, 
                    Token {getTokenType = OpenBracket, getTokenValue = "{"}, 
                    Token {getTokenType = CloseBracket, getTokenValue = "}"}, 
                    Token {getTokenType = Comma, getTokenValue = ","}, 
                    Token {getTokenType = Whitespace, getTokenValue = " "}, 
                    Token {getTokenType = OpenBracket, getTokenValue = "{"}, 
                    Token {getTokenType = Whitespace, getTokenValue = "                                   "}, 
                    Token {getTokenType = StringType, getTokenValue = "\"key6\""}, 
                    Token {getTokenType = Colon, getTokenValue = ":"}, 
                    Token {getTokenType = Whitespace, getTokenValue = " "}, 
                    Token {getTokenType = BooleanType, getTokenValue = "false"}, 
                    Token {getTokenType = Whitespace, getTokenValue = "                      "}, 
                    Token {getTokenType = CloseBracket, getTokenValue = "}"}, 
                    Token {getTokenType = CloseSquareBracket, getTokenValue = "]"}, 

                    Token {getTokenType = Whitespace, getTokenValue = "                                  "}, 
                    Token {getTokenType = CloseBracket, getTokenValue = "}"}
                  ]
  ]
