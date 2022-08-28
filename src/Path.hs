module Path where 

import Prelude

newtype Origin = Origin Path

instance Show Origin where
    show (Origin path) = show path

newtype Destination = Destination Path

instance Show Destination where
    show (Destination path) = show path

data Path 
    = PathDirectory Directories
    | PathFile Directories File 

instance Show Path where
    show (PathDirectory directories) = show directories 
    show (PathFile directories file_) = show directories ++ show file_

newtype Directories = Directories [Directory]

instance Show Directories where
    show (Directories dirs) =
        foldl (\acc dir -> acc ++ "/" ++ show dir) "" dirs

newtype Directory = Directory String

instance Show Directory where
    show (Directory name) = name

data File = File Name Extension 

instance Show File where
    show (File name extension) = "/" ++ show name ++ show extension

newtype Name = Name String 

instance Show Name where
    show (Name name) = name

data Extension
    = JS
    | HTML
    | JSON
    | PHP
    | CSS
    | HS
    | PURS
    | CABAL
    | MD

instance Show Extension where
    show JS = ".js"
    show HTML = ".html"
    show JSON = ".json"
    show PHP = ".php"
    show CSS = ".css"
    show HS = ".hs"
    show PURS = ".purs"
    show CABAL = ".cabal"
    show MD = ".md"


toString :: Path -> String 
toString = show

pathFile :: Directories -> File -> Path 
pathFile = PathFile

pathDirectories :: Directories -> Path
pathDirectories = PathDirectory

directory :: String -> Maybe Directory
directory str = 
    if null str 
    then Nothing 
    else Just $ Directory str

file :: String -> Extension -> Maybe File
file str ext =
    if null str
    then Nothing
    else Just $ File (Name str) ext