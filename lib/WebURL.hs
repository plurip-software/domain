module WebURL where

import Data.Maybe(catMaybes)
import qualified Path as P
import Data.List (intercalate)
import Data.List.Split (splitOn)

class Parser a where
    stringify :: a -> String
    parse     :: String -> Maybe a

data URL
    = URL Protocol Domain P.Path Arguments
    | URL' Protocol Domain P.Path
    | URL'' Protocol Domain

instance Show URL where
    show (URL protocol domain path arguments) =
        show protocol ++ "://" ++ show domain ++ "/" ++ show path ++ "?" ++ show arguments
    show (URL' protocol domain path) =
        show protocol ++ "://" ++ show domain ++ "/" ++ show path
    show (URL'' protocol domain) =
        show protocol ++ "://" ++ show domain

-- instance Parser URL where
--     stringify = show
--     parse urlString =  

        -- get arguments, rest --> split by "?"
        -- get partitionsRest ->  
            
newtype Protocol = Protocol Scheme

instance Show Protocol where
    show (Protocol scheme) = show scheme

data Domain
    = Domain Host TopLevel
    | DomainIP Host

instance Show Domain where
    show (Domain host toplevel) = show host ++ "." ++ show toplevel
    show (DomainIP host) = show host

instance Parser Domain where
    stringify = show
    parse domStr =
        if length domStr > 0 then
            case splitOn "." of
                [] -> Nothing
                host 
        else Nothing
    
data Host 
    = DNS String 
    | IP4 (Int, Int, Int, Int)

instance Show Host where
    show (DNS dns) = dns 
    show (IP4 (ip40, ip41, ip42, ip43)) = show ip40 ++ "." ++ show ip41 ++ "." ++ show ip42 ++ "." ++ show ip43

instance Parser Host where
    stringify = show
    parse hostStr = 
        if length hostStr > 0 then
            case splitOn "." hostStr of
                [ip40, ip41, ip42, ip43] -> 
                    case map readMaybe $ [ip40, ip41, ip42, ip43] of
                        [Just ip40_, Just ip41_, Just ip42_, Just ip43_] -> Just $ IP4 (ip40_, ip41_, ip42_, ip43_)
                        _ -> Nothing
                dns                      -> Just . DNS . intercalate "." $ dns 
        else Nothing

data TopLevel
    = Com
    | Org
    | Edu
    | Gov
    | Net
    | De

instance Show TopLevel where
    show Com = "com" 
    show Org = "org" 
    show Edu = "edu" 
    show Gov = "gov" 
    show Net = "net" 
    show De  = "de" 

instance Parser TopLevel where
    stringify = show
    parse "com" = Just Com 
    parse "org" = Just Org 
    parse "edu" = Just Edu 
    parse "gov" = Just Gov 
    parse "net" = Just Net 
    parse "de" = Just De 
    parse _ = Nothing

data Scheme 
    = HTTP
    | HTTPS

instance Show Scheme where
    show HTTP = "http"
    show HTTPS = "https"

instance Parser Scheme where
    stringify = show
    parse "http"  = Just HTTP
    parse "https" = Just HTTPS
    parse _ = Nothing

newtype Arguments = Arguments [Argument]

instance Show Arguments where
    show (Arguments arguments) = intercalate "&" (map show arguments)

instance Parser Arguments where
    stringify = show
    parse argsStr = 
        case splitOn "&" argsStr of
            []   -> Nothing
            args -> Just . Arguments. catMaybes $ map (\arg -> parse arg :: Maybe Argument) args

newtype Argument = Argument (Key, Value)

instance Show Argument where
    show (Argument (key, value)) = show key ++ "=" ++ show value

instance Parser Argument where
    stringify = show
    parse argStr = 
        case splitOn "=" argStr of
            [key, value] -> Just $ Argument (Key key, Value value)
            _            -> Nothing

newtype Key = Key String

instance Show Key where
    show (Key key) = key

newtype Value = Value String

instance Show Value where
    show (Value value) = value

https :: Protocol 
https = Protocol HTTPS

dotCom :: TopLevel
dotCom = Com

urlDNS' :: Scheme -> String -> TopLevel -> P.Path -> Maybe URL
urlDNS' scheme_ host_ topLvl path_ =
    case domainDNS host_ topLvl of
        Just domain_ -> Just $ URL' (Protocol scheme_) domain_ path_
        _            -> Nothing  

domainDNS :: String -> TopLevel -> Maybe Domain 
domainDNS host_ topLvl = 
    if length host_ > 1 
    then Just $ Domain (DNS host_) topLvl
    else Nothing

domainIP :: (Int, Int, Int, Int) -> Domain
domainIP ip4 = 
    DomainIP $ IP4 ip4