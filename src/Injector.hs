module Injector (inject) where

import Data.Char (isSpace)
import Data.Generics (everywhere, mkT)
import Data.List (intersperse)
import Language.JavaScript.Parser

import Config
import Profiler

inject :: Config -> String -> [String] -> JSNode -> JSNode
inject config fname contents = prepend (profiler config)
                             . everywhere (mkT $ f fname contents)

f :: String -> [String] -> Node -> Node
-- function test() { body; } -> function test() { start("test"); body; end(); }
f fname contents (JSFunction fn name lb args rb (NN (JSBlock a b c)))
  = JSFunction fn name lb args rb
      $ NN (JSBlock a (start fname contents (getPos fn) (extractName [name]) : b ++ [ jssemicolon, end ]) c)
-- function() { body; }; -> function() { start("anonymous"); body; end(); };
f fname contents (JSFunctionExpression fn name lb args rb (NN (JSBlock a b c)))
  = JSFunctionExpression fn name lb args rb
      $ NN (JSBlock a (start fname contents (getPos fn) (extractName name) : b ++ [ jssemicolon, end ]) c)
-- var test = function() { start("anonymous"); body; end(); }; -> function() { start("test"); body; end(); };
f fname contents
  (JSVarDecl
     variable
     [ equal@(NT (JSLiteral "=") _ _),
       NN (JSFunctionExpression fn name lb args rb
             (NN (JSBlock a (NN (JSVariables _ (NN (JSVarDecl name' _):_) _):b) c))) ])
  | extractName [name'] == identifier "state"
    = JSVarDecl
        variable
        [ equal,
          NN (JSFunctionExpression fn name lb args rb
               $ NN (JSBlock a (start fname contents (getPos fn) (extractName [variable])
                               : b ++ [ jssemicolon ]) c)) ]
-- var test = function() { body; }; -> function() { start("test"); body; end(); };
f fname contents
  (JSVarDecl
    variable
    [ equal@(NT (JSLiteral "=") _ _),
      NN (JSFunctionExpression fn name lb args rb
            (NN (JSBlock a b c))) ])
  = JSVarDecl
      variable
      [ equal,
        NN (JSFunctionExpression fn name lb args rb
           $ NN (JSBlock a (start fname contents (getPos fn) (extractName [variable])
                           : b ++ [ jssemicolon, end ]) c)) ]
-- throw expr; -> throw (function(arguments) { start(); var value = expr; end(); return value; }).call(this, arguments);
f _ _ (JSThrow throw expr)
  = JSThrow throw
      $ jscallNoSemicolon
        (jsmemberdot "call" $
          jsparen $
            jsfunction ["arguments"]
              [ jsvar (identifier "return") [expr],
                end,
                jsreturn (jsexpr $ jsidentifier (identifier "return")) ])
              [jsliteral "this", jsliteral "arguments"]
-- return expr; -> return (function(arguments) { start(); var value = expr; end(); return value; }).call(this, arguments);
f _ _ (JSReturn ret expr _)
  = JSReturn ret
      [ jscallNoSemicolon
        (jsmemberdot "call" $
          jsparen $
            jsfunction ["arguments"] $
              if null expr
                 then [ end ]
                 else [ jsvar (identifier "return") expr,
                        end,
                        jsreturn (jsexpr $ jsidentifier (identifier "return")) ])
                      [jsliteral "this", jsliteral "arguments"] ] jssemicolon
f _ _ x = x

identifier :: String -> String
identifier name = "sjsp__" ++ name

prepend :: String -> JSNode -> JSNode
prepend code node = NN $ JSExpression [ fromRight $ parse code "", node ]

start :: String -> [String] -> TokenPosn -> String -> JSNode
start fname contents (TokenPn _ line col) name
  = jsvar (identifier "state") [ jscallNoSemicolon (jsidentifier (identifier "start"))
                                 [ jsstring fname,
                                   jsnumber line,
                                   jsnumber col,
                                   jsstring name,
                                   jsstring (dropWhile isSpace $ contents !! (line - 1))] ]

end :: JSNode
end
  = NN $ JSExpression
           [ jsidentifier (identifier "end"),
             NN $ JSArguments (jsliteral "(") [jsidentifierNoSpace (identifier "state")] (jsliteral ")"),
             jssemicolon ]

extractName :: [JSNode] -> String
extractName [NT (JSIdentifier name) _ _] = name
extractName [NN (JSIdentifier name)] = name
extractName _ = "anonymous"

jsnumber :: Int -> JSNode
jsnumber n = NT (JSDecimal (show n)) pos []

jsstring :: String -> JSNode
jsstring xs = NT (JSStringLiteral '"' (tail $ init $ show xs)) pos []

jsfunction :: [String] -> [JSNode] -> JSNode
jsfunction xs node
  = NN $ JSFunctionExpression
           (jsliteral "function")
           []
           (jsliteral "(")
           (jscommas [NT (JSIdentifier x) pos [] | x <- xs ])
           (jsliteral ")")
           (NN $ JSBlock [jsliteral "{"] node [NT (JSLiteral "}") pos jsspace])

jscommas :: [JSNode] -> [JSNode]
jscommas = intersperse $ jsliteral ","

jsvar :: String -> [JSNode] -> JSNode
jsvar name expr
  = NN $ JSVariables
           (NT (JSLiteral "var") pos jsspace)
           [NN $ JSVarDecl
                  (NT (JSIdentifier name) pos jsspace)
                  (if null expr
                      then []
                      else [NT (JSLiteral "=") pos jsspace, NN (JSExpression expr)])]
           jssemicolon

jsidentifier :: String -> JSNode
jsidentifier name = NT (JSIdentifier name) pos jsspace

jsidentifierNoSpace :: String -> JSNode
jsidentifierNoSpace name = NT (JSIdentifier name) pos []

jsexpr :: JSNode -> JSNode
jsexpr = NN . JSExpression . (:[])

jsparen :: JSNode -> JSNode
jsparen expr
  = NN $ JSExpressionParen
           (NT (JSLiteral "(") pos jsspace)
           expr
           (NT (JSLiteral ")") pos jsspace)

jscallNoSemicolon :: JSNode -> [JSNode] -> JSNode
jscallNoSemicolon expr args
  = NN $ JSExpression
         [ expr, NN (JSArguments (jsliteral "(") (jscommas args) (jsliteral ")")) ]

jsreturn :: JSNode -> JSNode
jsreturn expr
  = NN $ JSReturn (NT (JSLiteral "return") pos jsspace) [expr] jssemicolon

jsmemberdot :: String -> JSNode -> JSNode
jsmemberdot name expr
  = NN $ JSMemberDot [expr]
         (jsliteral ".")
         (NT (JSIdentifier name) pos [])

jssemicolon :: JSNode
jssemicolon = jsliteral ";"

jsliteral :: String -> JSNode
jsliteral name = NT (JSLiteral name) pos []

jsspace :: [CommentAnnotation]
jsspace = [WhiteSpace pos " "]

getPos :: JSNode -> TokenPosn
getPos (NT _ p _) = p
getPos _ = pos

pos :: TokenPosn
pos = TokenPn 0 0 0

fromRight :: Either a b -> b
fromRight (Right x) = x
fromRight _ = undefined
