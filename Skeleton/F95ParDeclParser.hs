module F95ParDeclParser 
where
import F95Types
import Text.ParserCombinators.Parsec hiding (State)
import Text.ParserCombinators.Parsec.Expr
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language
import F95VarDeclParser

-- parse a parameter declaration string into a ParDecl 
f95_par_decl_parser :: Parser ParDecl
f95_par_decl_parser =
	do
		whiteSpace
		varType <- type_parser
		try(do whiteSpace; comma; reserved "parameter";)
		whiteSpace
		ranges <- try(do comma; whiteSpace; reserved "dimension"; whiteSpace; x <- parens dim_parser; return x) <|> (do return [])
		whiteSpace
		reserved "::"
		whiteSpace
		varName <- identifier
		whiteSpace
		reserved "="
		whiteSpace
		varVal <- expr_parser
		whiteSpace		
		return $ MkParDecl varType ranges varName varVal

