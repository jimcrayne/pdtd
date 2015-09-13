module Pdtd where

import Text.XML.HaXml.Parse (dtdParse)
import Text.XML.HaXml.Types (DocTypeDecl(..), MarkupDecl(..))
import Text.XML.HaXml.DtdToHaskell.Convert (dtd2TypeDef)
import Text.XML.HaXml.DtdToHaskell.TypeDef (ppTypeDef)
import Data.List (intersperse)
import Text.PrettyPrint (text)
import Control.Exception (bracket)
import System.IO (openFile, IOMode(..), hPrint, hClose)
--import System.Environment (getArgs, getProgName)
import System.FilePath (takeBaseName)
import Text.Printf (printf)

{-
 - Some types for quick reference
dtdParse :: String -> String -> Maybe DocTypeDecl
data DocTypeDecl = DTD Name (Maybe ExternalID) [MarkupDecl]  deriving Eq
dtd2TypeDef :: [MarkupDecl] -> [TypeDef]
ppTypeDef :: TypeDef -> Doc
-}

header1 n = text (printf "module %s where\n\nimport Text.XML.HaXml.XmlContent.Parser (List1)\n" n)
header2 n = text (printf "module %s where\n\nimport Data.List.NonEmpty\n\ntype List1 a = NonEmpty a\n" n)

pdtd dtdFile outFile = bracket (openFile outFile WriteMode) hClose $ \h -> do
        cts <- readFile dtdFile
        mapM_ (hPrint h) $ maybe (error "E: parse") (([header2 (takeBaseName outFile)] ++) . intersperse (text "\n") . map ppTypeDef . dtd2TypeDef . (\(DTD _ _ x) -> x)) (dtdParse "x86reference.dtd" cts) 
