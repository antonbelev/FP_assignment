module F95VarDeclParser where
import F95Types
import Text.ParserCombinators.Parsec hiding (State)
import Text.ParserCombinators.Parsec.Expr
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language

-- Run a parser p on a string str and print the result
run_parser_print :: Show a => Parser a -> String -> IO ()
run_parser_print p str = do
      case parse p "" str of
           Left err -> do
               putStr "parse error at "
               print err
           Right x  -> putStrLn $ "    "++(show x)++","
                                                                                                                                                         
-- Run a parser p on a string str and return the result
run_parser :: Parser a -> String -> a
run_parser p str =  case parse p "" str of
    Left err -> error $ "parse error at " ++ (show err)
    Right val  -> val  

f95_var_decl_parser :: Parser VarDecl
f95_var_decl_parser =
	do
		whiteSpace
		varType <- type_parser
		whiteSpace
		ranges <- try(do comma; whiteSpace; reserved "dimension"; whiteSpace; x <- parens dim_parser; return x) <|> (do return [])
		whiteSpace
		intent <- try(do comma; whiteSpace; x <- intent_parser; return x) <|> (do return InOut)
		whiteSpace
		vars <- arglist_parser
		whiteSpace
		mode <- try(ocl_argmode_parser) <|> (do return $ ReadWrite)
		return $ MkVarDecl varType ranges intent vars mode False False []

type_parser :: Parser VarType
type_parser =
	do
		whiteSpace
		t <- try (string "real") <|> string "integer"
		whiteSpace
		x <- try(parens $ do
			try(do whiteSpace; reserved "kind"; whiteSpace; char '='; whiteSpace; n<-integer; whiteSpace; return n)
			<|> try(do whiteSpace; n<-integer; whiteSpace; return n)
			<|> do whiteSpace; return 4) <|> return 4
		case t of 
			"real" ->  return $ MkVarType F95Real x
			"integer" ->  return $ MkVarType F95Integer x
      
dim_parser :: Parser [Range]
dim_parser =
	do
		ranges <- commaSep range_parser
		return ranges
		
range_parser :: Parser Range
range_parser =
		try(range_expr)
		<|> try(single_expr_range)
		<|> try(single_const_range)
		<|> single_var_range

single_var_range :: Parser Range    
single_var_range =
	do
		whiteSpace
		var1 <- var_expr
		whiteSpace
		return $ MkRange (Const 1) var1

single_const_range :: Parser Range
single_const_range =
	do
		whiteSpace
		const1 <- const_expr
		whiteSpace
		return $ MkRange (Const 1) const1

single_expr_range :: Parser Range
single_expr_range =
	do
		whiteSpace
		expr1 <- expr_parser
		whiteSpace
		return $ MkRange (Const 1) expr1

range_expr :: Parser Range    
range_expr = 
	do
		whiteSpace
		expr1 <- expr_parser
		whiteSpace
		char ':'
		whiteSpace
		expr2 <- expr_parser
		whiteSpace
		return $ MkRange expr1 expr2

intent_parser :: Parser Intent    
intent_parser = 
	do 
		whiteSpace
		reserved "intent" 
		whiteSpace
		x <- try(parens $ do
			whiteSpace; x <- (try( string "inout") <|> try(string "in") <|> string "out")
			whiteSpace; return $ x) <|> try(do return "inout")			
		case x of 
			"in" ->  return In
			"out" ->  return Out
			"inout" ->  return InOut
		
arglist_parser :: Parser [VarName]    
arglist_parser = 
	do
		whiteSpace
		string "::"
		whiteSpace
		args <- commaSep identifier
		return args
		
ocl_argmode_parser :: Parser OclArgMode    
ocl_argmode_parser =
	do
		whiteSpace
		reserved "!$acc"
		whiteSpace
		reserved "argmode"
		whiteSpace
		mode <- try(string "readwrite") <|> try(string "read") <|> string "write"
		case mode of 
			"read" ->  return Read
			"readwrite" ->  return ReadWrite
			"write" ->  return Write		

-- Parser for a term in expression as used e.g. in the dimension() attribute. 
-- This is not a dummy
term :: Parser Expr
term = parens expr_parser <|> const_expr <|> var_expr <?> "simple expression"
      
-- Parser for an expression as used e.g. in the dimension() attribute. 
-- This is not a dummy
expr_parser :: Parser Expr
expr_parser = buildExpressionParser optable term <?> "expression"

-- parser for a constant, e.g. 42
const_expr :: Parser Expr
const_expr =
	do
		x <- integer
		return $ Const (x)

-- parser for a variable e.g. v
var_expr :: Parser Expr
var_expr = 
	do
		x <- identifier
		return $ Var (x)

-- I suggest you don't touch the code below. It is not dummy code.
optable =
    let
        binop name assoc   = Infix ( do {  reservedOp name; return (\x y ->(Op (MkOpExpr name x y))) } ) assoc
        prefix name     = Prefix  ( reservedOp  name >> return (\x ->(Pref (MkPrefixOpExpr name x))) ) 
    in
        [ 
          [ prefix "-" ],
          [ binop "*"  AssocLeft, binop "/"  AssocLeft, binop "%" AssocLeft ]
        , [ binop "+"  AssocLeft, binop "-"  AssocLeft ]
        ]

lexer       = P.makeTokenParser emptyDef    

parens          = P.parens lexer    
commaSep        = P.commaSep lexer
commaSep1       = P.commaSep1 lexer
whiteSpace      = P.whiteSpace lexer    
symbol          = P.symbol lexer    
word            = P.identifier lexer
identifier      = P.identifier lexer
reserved        = P.reserved lexer    
reservedOp      = P.reservedOp lexer
integer         = P.integer lexer    
charLiteral     = P.charLiteral lexer    
stringLiteral   = P.stringLiteral lexer    
comma           = P.comma lexer
semi            = P.semi lexer
natural         = P.natural lexer

