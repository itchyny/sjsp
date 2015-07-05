module Config where

data Flag = Interval String
          | Top String
          | Accurate
          | Print
          | Version
          | Help
          deriving (Eq, Show)

data Config
  = Config { interval :: Integer
           , top :: Integer
           , accurate :: Bool
           }

isInterval :: Flag -> Bool
isInterval (Interval _) = True
isInterval _ = False

getInterval :: Flag -> Integer
getInterval (Interval x) = read x
getInterval _ = undefined

isTop :: Flag -> Bool
isTop (Top _) = True
isTop _ = False

getTop :: Flag -> Integer
getTop (Top x) = read x
getTop _ = undefined
