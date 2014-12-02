module Main where
import F95SrcIO ( read_F95_src, write_F95_src )
import F95OpenACCParser ( extract_OpenACC_regions_from_F95_src )
import F95VarDeclParser ( run_parser, f95_var_decl_parser )
import F95ParDeclParser ( f95_par_decl_parser )
import F95Types
import OpenCLAPIGenerator ( gen_OpenCL_API_calls )
import EvalExpr    
import qualified Data.Map as H
import F95VarDeclParser

{-
Anton Belev 
1103816b

Functional Programming 4

Assessed Exercise 1 (Mandatory): Parsing, Code Generation and State Manipulation in Haskell: a Real-world Application

Date
-}

{-
Status report:

Report the status of your submission. Which parts did you do? Does your solution work? What bugs are you aware of? Which extensions, if any, did you implement?
        
-}

{-

Sequence of actions:

 1/ detect arguments and argmodes, make a list of arguments

 2/ detect entry for new declarations for buffers and sizes, generate code and insert 

 3/ detect entry for API calls, generate code and insert 

-}

templ_src_name=  "module_LES_ocl_TEMPL.f95"
gen_src_name = "module_LES_ocl.f95"
gen_src_name_test = "laina_s_mlqko.f95"

      
-- ###############################################################################
-- Code for parsing the argument declarations

-- Given the lines containing arguments arg_lines and the lines containing 
-- the constant arguments const_arg_lines
-- create a table with as key the variable name and as value the parsed declaration
-- also returns a list of the argument variable names and the constant argument variable names
parse_arg_decls :: [String] -> [String] -> (ArgTable,[String],[String])
parse_arg_decls arg_lines const_arg_lines =
	let 
		at = H.empty
		varDeclsArg = [run_parser f95_var_decl_parser s | s <- arg_lines]
		varDeclsConstArg = [run_parser f95_var_decl_parser s | s <- const_arg_lines]
		at1 = add_to_argtable varDeclsArg at
		at2 = add_to_argtable varDeclsConstArg at1
		arg_names = [y | x <- varDeclsArg, y <- vd_varlist x]
		arg_const_name = [y | x <- varDeclsConstArg, y <- vd_varlist x]
	in (at2, arg_names, arg_const_name)
	
add_to_argtable :: [VarDecl] -> ArgTable -> ArgTable
add_to_argtable [] at = at
add_to_argtable varDecls at = let
		x = head varDecls
		varNames = vd_varlist x
		at1 = add_head_var_to_argtable varNames x at
		at2 = add_to_argtable (tail varDecls) at1	
	in at2
	
add_head_var_to_argtable :: [VarName] -> VarDecl -> ArgTable -> ArgTable
add_head_var_to_argtable [] varDecl at = at
add_head_var_to_argtable varNames varDecl at = let
		name = head varNames
		a = add_head_var_to_argtable (tail varNames) varDecl at
		at' = H.insert name varDecl a
	in at'

-- Given the parameter declarations, create a table with as key the parameter 
-- name and as value the parsed declaration	
parse_par_decls :: [String] -> VarTable    
parse_par_decls par_lines = 
	let 
		vt = H.empty
		parDecls = [run_parser f95_par_decl_parser s | s <- par_lines]
		vt' = add_to_vartable parDecls vt
	in vt'
	
add_to_vartable :: [ParDecl] -> VarTable -> VarTable
add_to_vartable [] vt = vt
add_to_vartable parDecls vt = let
		x = head parDecls
		v = add_to_vartable (tail parDecls) vt
		vt' = H.insert (pd_parname x) (pd_parval x) v
	in vt'

-- This takes a range expression and returns a tuple with the variable name and the 
-- computed size
eval_range_expr :: ArgTable -> VarTable -> String -> (String, [Integer])
eval_range_expr ocl_args par_table var_name = ("DUMMY",[])
       
-- ###############################
main :: IO ()
main = do 
	putStr $ unlines [
		"-- read source template from file"
		,"-- extract OpenACC regions"
		,"-- parse declarations"
		,"-- compute sizes for OpenCL arguments (this is hard, leave for last)"
		,"-- generate the target source code" 
		,"-- write generated source to file"
		]
	fileLines <- read_F95_src templ_src_name
	let (arg_lines, const_arg_lines, params) = extract_OpenACC_regions_from_F95_src fileLines
	--let a = gen_OpenCL_API_calls H.empty ["asd1", "asd2"] [] fileLines ""
	--write_F95_src gen_src_name_test a 
	
	let (argTable, arg_names, const_arg_names) = parse_arg_decls arg_lines const_arg_lines
	let gen_source = gen_OpenCL_API_calls argTable arg_names const_arg_names fileLines ""
	write_F95_src gen_src_name_test gen_source 
	
	--write_F95_src gen_src_name_test b
	--run_parser_print f95_par_decl_parser " integer, parameter  :: kp = 90  "
	
	--putStr $ show $ do parse_par_decls ["integer, parameter  :: jp = 150", "integer, parameter  :: kp = 90"]

	--let (a,b,c) = parse_arg_decls ["real(kind=4), dimension(0:3,0:ip+1,-1:jp+1,-1:kp+1)  :: uvw", "real(kind=4) :: dt"] ["integer :: im"]
	--putStr $ show c