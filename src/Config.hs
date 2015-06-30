module Config where

data Flag = Interval String
          | Print
          | Version
          | Help
          deriving (Eq, Show)

data Config
  = Config { interval :: Integer
           }

isInterval :: Flag -> Bool
isInterval (Interval x) = True
isInterval _ = False

getInterval :: Flag -> Integer
getInterval (Interval x) = read x
getInterval _ = undefined
