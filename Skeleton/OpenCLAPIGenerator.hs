module OpenCLAPIGenerator (
    gen_OpenCL_API_calls
        ) where
import F95Types
import Text.Regex.Posix -- suggest use of regular expressions
import Data.Char
import qualified Data.Map as H (lookup)
import Data.List

import System.Process -- only for localtime, entirely optional
import System.IO.Unsafe (unsafePerformIO) -- only for localtime, entirely optional

gen_OpenCL_API_calls :: ArgTable -> [String] -> [String] -> [String] -> String -> [String]    
gen_OpenCL_API_calls ocl_args arg_names const_arg_names src_lines templ_src_name 
	| length src_lines == 0 = []
	| isInfixOf "!$acc constarguments" (head src_lines) || isInfixOf "!$acc end constarguments" (head src_lines) 
		= gen_OpenCL_API_calls ocl_args arg_names const_arg_names (tail src_lines) templ_src_name
	| isInfixOf "!$acc arguments" (head src_lines) || isInfixOf "!$acc end arguments" (head src_lines) 
		= gen_OpenCL_API_calls ocl_args arg_names const_arg_names (tail src_lines) templ_src_name
	| isInfixOf "!$acc bufdecls" (head src_lines) = 
		gen_bufdecls arg_names ++
		gen_OpenCL_API_calls ocl_args arg_names const_arg_names (tail src_lines) templ_src_name
	| isInfixOf "!$acc sizedecls" (head src_lines) = 
		gen_sizedecls ocl_args arg_names ++
		gen_OpenCL_API_calls ocl_args arg_names const_arg_names (tail src_lines) templ_src_name
	| isInfixOf "!$acc makesizes" (head src_lines) = 
		gen_makesizes ocl_args arg_names const_arg_names src_lines templ_src_name ++
		gen_OpenCL_API_calls ocl_args arg_names const_arg_names (tail src_lines) templ_src_name
	| isInfixOf "!$acc makebuffers" (head src_lines) = 
		gen_makebuffers ocl_args arg_names ++
		gen_OpenCL_API_calls ocl_args arg_names const_arg_names (tail src_lines) templ_src_name
	| isInfixOf "!$acc setargs" (head src_lines) = 
		gen_setargs ocl_args arg_names const_arg_names ++
		gen_OpenCL_API_calls ocl_args arg_names const_arg_names (tail src_lines) templ_src_name
	| isInfixOf "!$acc writebuffers" (head src_lines) = 
		gen_writebuffers ocl_args arg_names ++
		gen_OpenCL_API_calls ocl_args arg_names const_arg_names (tail src_lines) templ_src_name
	| otherwise = [head src_lines] ++ gen_OpenCL_API_calls ocl_args arg_names const_arg_names (tail src_lines) templ_src_name

gen_bufdecls ::[String] -> [String]    
gen_bufdecls arg_names = [ "integer(8) :: " ++ arg ++ "_buf" | arg <- arg_names]

gen_sizedecls :: ArgTable -> [String] -> [String]    
gen_sizedecls ocl_args arg_names = 
	["integer, dimension(" ++ show (length (vd_dimension $ vardecl_lookup ocl_args arg)) 
	++ ") :: " ++ arg ++ "_sz"| arg <- arg_names]

vardecl_lookup :: ArgTable -> String -> VarDecl
vardecl_lookup ocl_args arg = let
	x = H.lookup arg ocl_args
	varDecl = case x of
		Nothing -> dummyVarDecl
		Just v -> v
	in varDecl

gen_makesizes :: ArgTable -> [String] -> [String] -> [String] -> String -> [String]    
gen_makesizes ocl_args arg_names const_arg_names src_lines templ_src_name = [ arg ++ "_sz" ++ " = shape(" ++ arg ++ ")" | arg <- arg_names]

gen_makebuffers :: ArgTable -> [String] -> [String]    
gen_makebuffers ocl_args arg_names = [ "call oclMake" ++ show (length (vd_dimension (vardecl_lookup ocl_args arg) )) 
	++ "D" ++ get_c_type (vd_vartype (vardecl_lookup ocl_args arg))
	++ "Array" ++ show (vd_argmode (vardecl_lookup ocl_args arg))
	++ "Buffer(" ++ arg ++ "_buf, " ++ arg ++ "_sz, " ++ arg ++ ")"| arg <- arg_names]

gen_setargs :: ArgTable -> [String] -> [String] -> [String]    
gen_setargs ocl_args arg_names const_arg_names = let 
	(res1,c1) = gen_setargs_arg ocl_args arg_names 0
	(res2,c2) = gen_setargs_const_arg ocl_args const_arg_names c1
	in (res1 ++ res2)

gen_setargs_arg :: ArgTable -> [String] -> Integer -> ([String], Integer)  
gen_setargs_arg ocl_args [] count = ([],count) 
gen_setargs_arg ocl_args arg_names count = let 
	arg = head arg_names
	(r,c) = gen_setargs_arg ocl_args (tail arg_names) (count+1)
	res' = ["call oclSet" 
		++ get_c_type (vd_vartype (vardecl_lookup ocl_args arg))
		++ "ArrayArg(" ++ show count ++ ", " ++ arg ++ "_buf )"] ++ r
	in (res', c)

gen_setargs_const_arg :: ArgTable -> [String] -> Integer -> ([String], Integer)  
gen_setargs_const_arg ocl_args [] count = ([],count) 
gen_setargs_const_arg ocl_args const_arg_names count = let 
	arg = head const_arg_names
	(r,c) = gen_setargs_const_arg ocl_args (tail const_arg_names) (count+1)
	res' = ["call oclSet" 
		++ get_c_type (vd_vartype (vardecl_lookup ocl_args arg))
		++ "ConstArg(" ++ show count ++ ", " ++ arg ++ " )"] ++ r
	in (res', c)

gen_writebuffers :: ArgTable -> [String] -> [String]    
gen_writebuffers ocl_args arg_names = [ "call oclWrite" ++ show (length (vd_dimension (vardecl_lookup ocl_args arg) )) 
	++ "D" ++ get_c_type (vd_vartype (vardecl_lookup ocl_args arg))
	++ "ArrayBuffer(" ++ arg ++ "_buf, " ++ arg ++ "_sz, " ++ arg ++ ")"| arg <- arg_names]

get_c_type :: VarType -> String
get_c_type vt = let
	numType = at_numtype vt
	n = case numType of
		F95Integer -> "Int"
		F95Real -> "Float"
	in n
                
ucfirst (x:xs)  = (toUpper x):xs

localtime = unsafePerformIO $ readProcess "/bin/date" [] []

