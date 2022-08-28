module Domain where

import Data.Maybe()
import qualified Path as P

data Domain
    = Domain Protocol Host TopLevel P.Path Arguments

instance Show Domain where
    show (Domain (Protocol scheme) host topLevel path arguments) =
        show scheme ++ show host ++ show topLevel ++ show path ++ "?" ++ show arguments
            
newtype Protocol = Protocol Scheme

instance Show Protocol where
    show (Protocol scheme) = show scheme
    
data Host 
    = DNS String
    | IP4 (Int, Int, Int, Int)

instance Show Host where
    show (DNS dns) = dns 
    show (IP4 (ip40, ip41, ip42, ip43)) = show ip40 ++ "." ++ show ip41 ++ "." ++ show ip42 ++ "." ++ show ip43

data TopLevel
    = Com
    | Org
    | Edu
    | Gov
    | Net
    | De

instance Show TopLevel where
    show Com = ".com" 
    show Org = ".org" 
    show Edu = ".edu" 
    show Gov = ".gov" 
    show Net = ".net" 
    show De  = ".de" 

data Scheme 
    = HTTP
    | HTTPS

instance Show Scheme where
    show HTTP = "http://"
    show HTTPS = "https://"

newtype Arguments = Arguments [Argument]

instance Show Arguments where
    show (Arguments arguments) = "/" ++ foldl (\acc arg -> acc ++ show arg) "" arguments ++ "a=1"

newtype Argument = Argument (Key, Value)

instance Show Argument where
    show (Argument (key, value)) = show key ++ "=" ++ show value ++ "&" 

newtype Key = Key String

instance Show Key where
    show (Key key) = key

newtype Value = Value String

instance Show Value where
    show (Value value) = value

argument :: Key -> Value -> Argument
argument key value =
    Argument (key, value)

domain :: Protocol -> Host -> TopLevel ->  P.Path -> Arguments -> Domain
domain =
    Domain 