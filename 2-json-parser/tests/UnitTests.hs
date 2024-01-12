module Tests where

import Tokenizer 
import Parser
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain unitTests 

unitTests :: TestTree
unitTests = testGroup "unit tests" [tokenizerTests, parserTests, fileTests]

parserTests :: TestTree 
parserTests = testGroup "parser tests" [ 
    testCase "parse simple json object" $
      createExpressionFrom "{}" @?= Right ObjectExpression { getFields = [] },
    testCase "parse simple json object with spaces" $
      createExpressionFrom "{  }" @?= Right ObjectExpression { getFields = [] },
    testCase "json object with string field" $ 
      createExpressionFrom "{\"hello\" : \"world\" }" @?= Right 
            ObjectExpression { getFields = [(StringValueExpression { getStringValue = "hello" }, StringExpression $ StringValueExpression { getStringValue = "world" })] },
    testCase "json object with fields of different types" $ 
      createExpressionFrom "{ \
                  \ \"key1\": true,       \
                  \ \"key2\": false,      \
                  \ \"key3\": null,       \
                  \ \"key4\": \"value\",  \
                  \ \"key5\": 101         \
                  \ }" @?= Right
            ObjectExpression { getFields = [
              (StringValueExpression { getStringValue = "key1" }, BooleanExpression { getBooleanValue = True }),
              (StringValueExpression { getStringValue = "key2" }, BooleanExpression { getBooleanValue = False }),
              (StringValueExpression { getStringValue = "key3" }, NullExpression ),
              (StringValueExpression { getStringValue = "key4" }, StringExpression $ StringValueExpression { getStringValue = "value" }),
              (StringValueExpression { getStringValue = "key5" }, NumberExpression { getNumberValue = 101 })
            ]},
    testCase "json object with fields of complex types" $ 
      createExpressionFrom " {    \
            \  \"key1\": \"value\",                 \
            \  \"key2\": 101,                       \
            \  \"key3\": { \"key5\": false },        \
            \  \"key4\": [                          \
            \     {}, {                             \
            \      \"key6\": false                  \
            \    }]                                 \
            \ }" @?= Right 
            ObjectExpression { getFields = [
              (StringValueExpression { getStringValue = "key1" }, StringExpression $ StringValueExpression { getStringValue = "value" }),
              (StringValueExpression { getStringValue = "key2" }, NumberExpression { getNumberValue = 101 }),
              (StringValueExpression { getStringValue = "key3" }, ObjectExpression { getFields = [
                   (StringValueExpression { getStringValue = "key5" }, BooleanExpression { getBooleanValue = False })
              ]}),
              (StringValueExpression { getStringValue = "key4" }, ArrayExpression { getArrayElements = [
                   ObjectExpression { getFields = [] }, 
                   ObjectExpression { getFields = [
                    (StringValueExpression { getStringValue = "key6" }, BooleanExpression { getBooleanValue = False })
                   ]}
              ]})
            ]}
  ]

tokenizerTests :: TestTree 
tokenizerTests = testGroup "tokenizer tests" [ 
    testCase "invalid str" $
      createTokenizer " !!! " @?= Left "can't parse token: '!!! '",
    testCase "empty string" $
      createTokenizer "  \
      \     \n\
      \ " @?= Right [ 
                    Token {getTokenType = Whitespace, getTokenValue = "       \n "} 
              ],
    testCase "empty json object" $
      createTokenizer "{}" @?= Right [
                    Token {getTokenType = OpenBracket, getTokenValue = "{"}, 
                    Token {getTokenType = CloseBracket, getTokenValue = "}"}
                  ],
    testCase "json object with spaces" $ 
      createTokenizer "{  }" @?= Right [ 
                    Token {getTokenType = OpenBracket, getTokenValue = "{"}, 
                    Token {getTokenType = Whitespace, getTokenValue = "  "}, 
                    Token {getTokenType = CloseBracket, getTokenValue = "}"}
                  ],
    testCase "json object with string field" $ 
      createTokenizer "{\"hello\" : \"world\" }" @?= Right [ 
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
            \  \"key5\": -100.1e+10 }" @?= Right [ 
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
            \ }" @?= Right [ 
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

fileTests :: TestTree 
fileTests = testGroup "parser tests" []

createExpressionFrom :: String -> Either String Expression
createExpressionFrom str = ((createTokenizer str) >>= (\tokenizer -> getObjectExpression tokenizer))