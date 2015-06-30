module Helper (apply) where

import Data.Functor ((<$>))
import Language.JavaScript.Parser

apply :: (Node -> Node) -> JSNode -> JSNode
apply = v

v :: (Node -> Node) -> JSNode -> JSNode
v f (NN x) = NN (w f x)
v f (NT x y z) = NT (w f x) y z

w :: (Node -> Node) -> Node -> Node
w u (JSArguments a b c) = u $ JSArguments (v u a) (v u <$> b) (v u c)
w u (JSArrayLiteral a b c) = u $ JSArrayLiteral (v u a) (v u <$> b) (v u c)
w u (JSBlock a b c) = u $ JSBlock (v u <$> a) (v u <$> b) (v u <$> c)
w u (JSBreak a b c) = u $ JSBreak (v u a) (v u <$> b) c
w u (JSCallExpression a b c d) = u $ JSCallExpression a (v u <$> b) (v u <$> c) (v u <$> d)
w u (JSCase a b c d) = u $ JSCase (v u a) (v u b) (v u c) (v u <$> d)
w u (JSCatch a b c d e f) = u $ JSCatch (v u a) (v u b) (v u c) (v u <$> d) (v u e) (v u f)
w u (JSContinue a b c) = u $ JSContinue (v u a) (v u <$> b) (v u c)
w u (JSDecimal a) = u $ JSDecimal a
w u (JSDefault a b c) = u $ JSDefault (v u a) (v u b) (v u <$> c)
w u (JSDoWhile a b c d e f g) = u $ JSDoWhile (v u a) (v u b) (v u c) (v u d) (v u e) (v u f) (v u g)
w u (JSElision a) = u $ JSElision (v u a)
w u (JSExpression a) = u $ JSExpression (v u <$> a)
w u (JSExpressionBinary a b c d) = u $ JSExpressionBinary a (v u <$> b) (v u c) (v u <$> d)
w u (JSExpressionParen a b c) = u $ JSExpressionParen (v u a) (v u b) (v u c)
w u (JSExpressionPostfix a b c) = u $ JSExpressionPostfix a (v u <$> b) (v u c)
w u (JSExpressionTernary a b c d e) = u $ JSExpressionTernary (v u <$> a) (v u b) (v u <$> c) (v u d) (v u <$> e)
w u (JSFinally a b) = u $ JSFinally (v u a) (v u b)
w u (JSFor a b c d e f g h i) = u $ JSFor (v u a) (v u b) (v u <$> c) (v u d) (v u <$> e) (v u f) (v u <$> g) (v u h) (v u i)
w u (JSForIn a b c d e f g) = u $ JSForIn (v u a) (v u b) (v u <$> c) (v u d) (v u e) (v u f) (v u g)
w u (JSForVar a b c d e f g h i j) = u $ JSForVar (v u a) (v u b) (v u c) (v u <$> d) (v u e) (v u <$> f) (v u g) (v u <$> h) (v u i) (v u j)
w u (JSForVarIn a b c d e f g h) = u $ JSForVarIn (v u a) (v u b) (v u c) (v u d) (v u e) (v u f) (v u g) (v u h)
w u (JSFunction a b c d e f) = u $ JSFunction (v u a) (v u b) (v u c) (v u <$> d) (v u e) (v u f)
w u (JSFunctionExpression a b c d e f) = u $ JSFunctionExpression (v u a) (v u <$> b) (v u c) (v u <$> d) (v u e) (v u f)
w u (JSHexInteger a) = u $ JSHexInteger a
w u (JSIdentifier a) = u $ JSIdentifier a
w u (JSIf a b c d e f) = u $ JSIf (v u a) (v u b) (v u c) (v u d) (v u <$> e) (v u <$> f)
w u (JSLabelled a b c) = u $ JSLabelled (v u a) (v u b) (v u c)
w u (JSLiteral a) = u $ JSLiteral a
w u (JSMemberDot a b c) = u $ JSMemberDot (v u <$> a) (v u b) (v u c)
w u (JSMemberSquare a b c d) = u $ JSMemberSquare (v u <$> a) (v u b) (v u c) (v u d)
w u (JSObjectLiteral a b c) = u $ JSObjectLiteral (v u a) (v u <$> b) (v u c)
w u (JSOctal a) = u $ JSOctal a
w u (JSOperator a) = u $ JSOperator (v u a)
w u (JSPropertyAccessor a b c d e f) = u $ JSPropertyAccessor (v u a) (v u b) (v u c) (v u <$> d) (v u e) (v u f)
w u (JSPropertyNameandValue a b c) = u $ JSPropertyNameandValue (v u a) (v u b) (v u <$> c)
w u (JSRegEx a) = u $ JSRegEx a
w u (JSReturn a b c) = u $ JSReturn (v u a) (v u <$> b) (v u c)
w u (JSSourceElementsTop a) = u $ JSSourceElementsTop (v u <$> a)
w u (JSStringLiteral a b) = u $ JSStringLiteral a b
w u (JSSwitch a b c d e) = u $ JSSwitch (v u a) (v u b) (v u c) (v u d) (v u e)
w u (JSThrow a b) = u $ JSThrow (v u a) (v u b)
w u (JSTry a b c) = u $ JSTry (v u a) (v u b) (v u <$> c)
w u (JSUnary a b) = u $ JSUnary a (v u b)
w u (JSVarDecl a b) = u $ JSVarDecl (v u a) (v u <$> b)
w u (JSVariables a b c) = u $ JSVariables (v u a) (v u <$> b) (v u c)
w u (JSWhile a b c d e) = u $ JSWhile (v u a) (v u b) (v u c) (v u d) (v u e)
w u (JSWith a b c d e) = u $ JSWith (v u a) (v u b) (v u c) (v u d) (v u <$> e)
