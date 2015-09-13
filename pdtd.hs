
import System.Environment (getArgs, getProgName)
import Text.Printf (printf)
import Pdtd

usage = do
    pn <- getProgName
    printf "Usage: %s FILE.dtd OUTFILE.hs\n" pn

main = do
    args <- getArgs
    let dtdFile=args !! 0
        outFile=args !! 1
    if length args /= 2 then usage else pdtd dtdFile outFile
