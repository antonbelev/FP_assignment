module EvalExpr (eval, VarTable) where
import F95Types
import qualified Data.Map as H
import Data.List

type VarTable = H.Map String Expr
-- given an expression and the variable lookup table, return the integer value 
-- of the evaluated expression and the updated table
eval :: Expr -> VarTable -> (Integer, VarTable)
eval (Const x) vtable = (x, vtable)
eval (Var x) vtable = let
	xVal = H.lookup x vtable
	expr = case xVal of
		Nothing -> Const 0
		Just e -> e
	(val,vt) = eval expr vtable
	vt' = H.insert x (Const val) vt
	in
		(val, vt')
eval (Op x) vtable = let
	val = eval_expr x vtable
	in 
		(val, vtable)
eval (Pref x) vtable = let
	val = eval_prefix_expr x vtable
	in 
		(val, vtable)
	
	
-- given a binary operator expression (e.g. x+y) and the variable lookup table, 
-- return the integer value of the evaluated expression
eval_expr :: OpExpr -> VarTable -> Integer    
eval_expr oe vt = let
	op = oe_op oe
	(le,_) = eval (oe_lhs oe) vt
	(re,_) = eval (oe_rhs oe) vt
	val = case op of
		"+" -> le + re
		"-" -> le - re
		"*" -> le * re
		"/" -> le `div` re
		"mod" -> le `mod` re
	in val
	
-- given a unary operator expression (e.g. -x) and the variable lookup table, 
-- return the integer value of the evaluated expression
eval_prefix_expr :: PrefixOpExpr -> VarTable -> Integer
eval_prefix_expr pe vt = let
	(val,_) = eval (poe_exp pe) vt
	in -val
		
