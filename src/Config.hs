module Config where

data Flag = Interval String
          | Number String
          | Accurate
          | Print
          | Version
          | Help
          deriving (Eq, Show)

data Config
  = Config { interval :: Integer
           , number :: Integer
           , accurate :: Bool
           }

isInterval :: Flag -> Bool
isInterval (Interval _) = True
isInterval _ = False

getInterval :: Flag -> Integer
getInterval (Interval x) = read x
getInterval _ = undefined

isNumber :: Flag -> Bool
isNumber (Number _) = True
isNumber _ = False

getNumber :: Flag -> Integer
getNumber (Number x) = read x
getNumber _ = undefined
