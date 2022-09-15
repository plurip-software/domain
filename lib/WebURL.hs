{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE StrictData   #-}

module WebURL where

import           Data.List       (intercalate)
import           Data.List.Split (splitOn)
import           Helper          (svLast)
import qualified Path            as P
import           Text.Read       (readMaybe)
import Data.Maybe (mapMaybe)

class Parser a where
    stringify :: a -> String
    parse     :: String -> Maybe a

data URL
    = URL Protocol Domain P.Path Arguments
    | URL' Protocol Domain P.Path
    | URL'' Protocol Domain

instance Show URL where
    show :: URL -> String
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
    show :: Protocol -> String
    show (Protocol scheme) = show scheme

newtype Domain = Domain Host 

instance Show Domain where
    show :: Domain -> String
    show (Domain host) = show host

instance Parser Domain where
    stringify = show
    parse "" = Nothing
    parse domStr =
        case splitOn "." domStr of
            [] -> Nothing
            _  -> 
                case (parse domStr :: Maybe Host) of
                    Just host_ -> Just $ Domain host_
                    _          -> Nothing

data Host
    = DNS String TopLevel
    | IP4 (Int, Int, Int, Int)

instance Show Host where
    show :: Host -> String
    show (DNS dns topLvl) = dns ++ "." ++ show topLvl
    show (IP4 (ip40, ip41, ip42, ip43)) = show ip40 ++ "." ++ show ip41 ++ "." ++ show ip42 ++ "." ++ show ip43

instance Parser Host where
    stringify :: Host -> String
    stringify = show

    parse :: String -> Maybe Host
    parse "" = Nothing
    parse hostStr =
        let
            spltStr :: Either [Int] [String]
            spltStr = 
                case mapMaybe (\str -> readMaybe str :: Maybe Int) $ splitOn "." hostStr of
                    []   -> Right $ splitOn "." hostStr
                    ints -> Left ints
        in
        if length spltStr == 4
        then
            case spltStr of
                Left [ip40, ip41, ip42, ip43] -> Just $ IP4 (ip40, ip41, ip42, ip43)
                Left _ -> Nothing
                Right spltStr_                ->
                    case svLast spltStr_ of
                        Just ext -> 
                            case (parse ext :: Maybe TopLevel) of
                                Nothing   -> Nothing
                                Just ext_ -> Just $ DNS (intercalate "."  spltStr_) ext_
                        Nothing -> Nothing
                        
        else Nothing

data TopLevel
    = Com
    | Org
    | Edu
    | Gov
    | Net
    | De

instance Show TopLevel where
    show :: TopLevel -> String
    show Com = "com"
    show Org = "org"
    show Edu = "edu"
    show Gov = "gov"
    show Net = "net"
    show De  = "de"

instance Parser TopLevel where
    stringify :: TopLevel -> String
    stringify = show

    parse :: String -> Maybe TopLevel
    parse "com" = Just Com
    parse "org" = Just Org
    parse "edu" = Just Edu
    parse "gov" = Just Gov
    parse "net" = Just Net
    parse "de"  = Just De
    parse _     = Nothing

data Scheme
    = HTTP
    | HTTPS

instance Show Scheme where
    show :: Scheme -> String
    show HTTP  = "http"
    show HTTPS = "https"

instance Parser Scheme where
    stringify :: Scheme -> String
    stringify = show

    parse :: String -> Maybe Scheme
    parse "http"  = Just HTTP
    parse "https" = Just HTTPS
    parse _       = Nothing

newtype Arguments = Arguments [Argument]

instance Show Arguments where
    show :: Arguments -> String
    show (Arguments arguments) = intercalate "&" (map show arguments)

instance Parser Arguments where
    stringify :: Arguments -> String
    stringify = show

    parse :: String -> Maybe Arguments
    parse argsStr =
        case splitOn "&" argsStr of
            []   -> Nothing
            args -> Just . Arguments $ mapMaybe parse args

newtype Argument = Argument (Key, Value)

instance Show Argument where
    show :: Argument -> String
    show (Argument (key, value)) = show key ++ "=" ++ show value

instance Parser Argument where
    stringify :: Argument -> String
    stringify = show

    parse :: String -> Maybe Argument
    parse argStr =
        case splitOn "=" argStr of
            [key, value] -> Just $ Argument (Key key, Value value)
            _            -> Nothing

newtype Key = Key String

instance Show Key where
    show :: Key -> String
    show (Key key) = key

newtype Value = Value String

instance Show Value where
    show :: Value -> String
    show (Value value) = value

https :: Protocol
https = Protocol HTTPS

dotCom :: TopLevel
dotCom = Com

urlDNS' :: Scheme -> String -> P.Path -> Maybe URL
urlDNS' scheme_ host_  path_ =
    case domainDNS host_ of
        Just domain_ -> Just $ URL' (Protocol scheme_) domain_ path_
        _            -> Nothing

domainDNS :: String -> Maybe Domain
domainDNS "" = Nothing
domainDNS host_ =
        parse host_

domainIP :: (Int, Int, Int, Int) -> Domain
domainIP =
    Domain . IP4
