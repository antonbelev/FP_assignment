module F95SrcIO (
		read_F95_src,
        write_F95_src
) where
import Data.Char (toLower)
-- Fortran is case-insensitive so turn everything into lowercase
lc = map toLower
-- given the name of the file, read it into a list of strings, one per line of source code
read_F95_src :: String -> IO [String]
read_F95_src src_name = 
	do
		contents <- readFile src_name
		return $ lines (lc contents)

-- given a list of strings, one per line of source code, and the name of the file, write the strings to the file
write_F95_src :: String -> [String] -> IO ()
write_F95_src src_name src_lines =
	do
	    writeFile src_name $ unlines src_lines  

    
