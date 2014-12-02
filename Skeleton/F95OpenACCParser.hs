module F95OpenACCParser (
    extract_OpenACC_regions_from_F95_src
) where
import Text.Regex.Posix -- suggest use of regular expressions
import Data.List
-- given the source code as a list of lines (strings), extract the OpenACC regions 
-- for Arguments and ConstArguments as well as the parameter declarations 
-- (i.e. _any_ variable declaration with the parameter attribute), and return 
-- them as a tuple of three lists of strings, in that order.
-- arguments, const arguments, paramter decl
extract_OpenACC_regions_from_F95_src :: [String] -> ([String],[String],[String])
extract_OpenACC_regions_from_F95_src in_src_lines = (extract_arguments False in_src_lines,extract_constarguments False in_src_lines, extract_params in_src_lines)

extract_arguments :: Bool -> [String] -> [String]
extract_arguments state line
	| length line == 0 = []
	| state &&  not (isInfixOf "!$acc end arguments" (head line)) = [head line] ++ extract_arguments True (tail line)
	| state = extract_arguments False (tail line)
	| isInfixOf "!$acc arguments" (head line) = extract_arguments True (tail line)
	| otherwise = extract_arguments False (tail line)

extract_constarguments :: Bool -> [String] -> [String]
extract_constarguments state line
	| length line == 0 = []
	| state &&  not (isInfixOf "!$acc end constarguments" (head line)) = [head line] ++ extract_constarguments True (tail line)
	| state = extract_constarguments False (tail line)
	| isInfixOf "!$acc constarguments" (head line) = extract_constarguments True (tail line)
	| otherwise = extract_constarguments False (tail line)
	
extract_params :: [String] -> [String]
extract_params line
	| length line == 0 = []
	| isInfixOf " parameter " (head line) = [head line] ++ extract_params (tail line)
	| otherwise = extract_params (tail line)

