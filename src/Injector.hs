module Injector (inject) where

import Data.Char (isSpace)
import Data.Generics (everywhere, mkT)
import Data.List (intersperse)
import Language.JavaScript.Parser

import Config

inject :: Config -> String -> [String] -> JSNode -> JSNode
inject config fname contents = profiler config . everywhere (mkT $ f fname contents)

f :: String -> [String] -> Node -> Node
-- function test() { body; } -> function test() { start("test"); body; end(); }
f fname contents (JSFunction fn name lb args rb (NN (JSBlock a b c)))
  = JSFunction fn name lb args rb
      $ NN (JSBlock a (start fname contents (getPos a) (extractName [name]) : b ++ [ jssemicolon, end ]) c)
-- function() { body; }; -> function() { start("anonymous"); body; end(); };
f fname contents (JSFunctionExpression fn name lb args rb (NN (JSBlock a b c)))
  = JSFunctionExpression fn name lb args rb
      $ NN (JSBlock a (start fname contents (getPos a) (extractName name) : b ++ [ jssemicolon, end ]) c)
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
               $ NN (JSBlock a (start fname contents (getPos a) (extractName [variable])
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
           $ NN (JSBlock a (start fname contents (getPos a) (extractName [variable])
                           : b ++ [ jssemicolon, end ]) c)) ]
-- throw expr; -> throw (function(arguments) { start(); var value = expr; end(); return value; }).call(this, arguments);
f _ _ (JSThrow throw expr)
  = JSThrow throw
      ( jscallNoSemicolon
        (jsmemberdot "call" $
          jsparen $
            jsfunction ["arguments"]
              [ jsvar (identifier "return") [expr],
                end,
                jsreturn (jsexpr $ jsidentifier (identifier "return")) ])
              [NT (JSLiteral "this") pos [], NT (JSLiteral "arguments") pos []] )
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
                      [NT (JSLiteral "this") pos [], NT (JSLiteral "arguments") pos []] ] jssemicolon
f _ _ x = x

identifier :: String -> String
identifier name = "sjsp__" ++ name

profiler :: Config -> JSNode -> JSNode
profiler config node = NN $ JSExpression [ fromRight $ flip parse "" $
  concat [ "window." ++ identifier "result" ++ " = window." ++ identifier "result" ++ " || {}; "
         , identifier "state" ++ " = { time: 0, line: 0, col: 0, name: '', fname: '', linestr: '' };"
         , identifier "start" ++ " = function(fname, line, col, name, linestr) {"
         , "  return { time: Date.now(), line: line, col: col, name: name, fname: fname, linestr: linestr };"
         , "};"
         , identifier "end" ++ " = function(x) {"
         , "  if (!x.time) return;"
         , "  var key = x.fname + ' :: ' + x.line + ' :: ' + x.col; "
         , "  " ++ identifier "result" ++ "[key] = " ++ identifier "result" ++ "[key] || { count: 0, time: 0, line: x.line, col: x.col, name: x.name, fname: x.fname, linestr: x.linestr }; "
         , "  " ++ identifier "result" ++ "[key].time += (Date.now() - x.time); "
         , "  " ++ identifier "result" ++ "[key].count += 1; "
         , "}; "
         , identifier "print" ++ " = function(x, n) { return Array(Math.max(0, n - x.toString().length + 1)).join(' ') + x; }; "
         , identifier "result_time" ++ " = []; "
         , identifier "result_count" ++ " = []; "
         , identifier "format" ++ " = function(x) { return"
         , " 'time: ' + " ++ identifier "print" ++ "((x.time / 1000).toFixed(2), 7) + 'sec  "
         , " count: ' + " ++ identifier "print" ++ "(x.count, 7) + ' '"
         , " + " ++ identifier "print" ++ "(x.fname, 15) + '  '"
         , " + " ++ identifier "print" ++ "(x.name, 13) + '  '"
         , " + ' (line:' + " ++ identifier "print" ++ "(x.line, 4) + ', col:' + " ++ identifier "print" ++ "(x.col, 3) + ')   ' + x.linestr; }; "
         , "if (window.hasOwnProperty('" ++ identifier "interval" ++ "')) { "
         , "  clearInterval(window." ++ identifier "interval" ++ ");"
         , "}"
         , "window." ++ identifier "interval" ++ " = setInterval(function() { "
         , "  console.log('========== SORT BY TIME ==========\\n' + "
         , " (" ++ identifier "result_time" ++ " = Object.keys(" ++ identifier "result" ++ ")"
         , ".map(function(key) { return " ++ identifier "result" ++ "[key]; })"
         , ".sort(function(x, y) { return y.time - x.time; })"
         , ".slice(0, 20)"
         , ".map(function(x){ return " ++ identifier "format" ++ "(x); })).join('\\n') + "
         , "  '\\n========== SORT BY COUNT ==========\\n' + "
         , " (" ++ identifier "result_count" ++ " = Object.keys(" ++ identifier "result" ++ ")"
         , ".map(function(key) { return " ++ identifier "result" ++ "[key]; })"
         , ".sort(function(x, y) { return y.count - x.count; })"
         , ".slice(0, 20)"
         , ".map(function(x){ return " ++ identifier "format" ++ "(x); })).join('\\n')); "
         , "}, " ++ show (interval config) ++ " * 1000);" ], node ]

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
             NN $ JSArguments (NT (JSLiteral "(") pos []) [jsidentifierNoSpace (identifier "state")] (NT (JSLiteral ")") pos []),
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
           (NT (JSLiteral "function") pos [])
           []
           (NT (JSLiteral "(") pos [])
           (jscommas [NT (JSIdentifier x) pos [] | x <- xs ])
           (NT (JSLiteral ")") pos [])
           (NN $ JSBlock [NT (JSLiteral "{") pos []] node [NT (JSLiteral "}") pos jsspace])

jscommas :: [JSNode] -> [JSNode]
jscommas = intersperse $ NT (JSLiteral ",") pos []

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

-- jscall :: JSNode -> [JSNode] -> JSNode
-- jscall expr args
--   = NN $ JSExpression
--          [ expr, NN (JSArguments (NT (JSLiteral "(") pos []) args (NT (JSLiteral ")") pos [])), jssemicolon ]

jscallNoSemicolon :: JSNode -> [JSNode] -> JSNode
jscallNoSemicolon expr args
  = NN $ JSExpression
         [ expr, NN (JSArguments (NT (JSLiteral "(") pos []) (jscommas args) (NT (JSLiteral ")") pos [])) ]

jsreturn :: JSNode -> JSNode
jsreturn expr
  = NN $ JSReturn (NT (JSLiteral "return") pos jsspace) [expr] jssemicolon

jsmemberdot :: String -> JSNode -> JSNode
jsmemberdot name expr
  = NN $ JSMemberDot [expr]
         (NT (JSLiteral ".") pos [])
         (NT (JSIdentifier name) pos [])

jssemicolon :: JSNode
jssemicolon = NT (JSLiteral ";") pos []

jsspace :: [CommentAnnotation]
jsspace = [WhiteSpace pos " "]

getPos :: [JSNode] -> TokenPosn
getPos [NT _ p _] = p
getPos _ = pos

pos :: TokenPosn
pos = TokenPn 0 0 0

fromRight :: Either a b -> b
fromRight (Right x) = x
fromRight _ = undefined
