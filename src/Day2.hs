module Day2
    ( Range
    , Password
    , day2a
    , day2b
    ) where

day2a :: [Password] -> Int
day2a ps = length $ filter validatePassword ps

day2b :: [Password] -> Int
day2b ps = length $ filter validatePassword2 ps

validatePassword :: Password -> Bool
validatePassword (Password (Range l r) cs password) =
  l <= count && count <= r
  where
    c = head cs
    count = length $ filter (== c) password

validatePassword2 :: Password -> Bool
validatePassword2 (Password (Range b e) cs password) =
  (x == c && y /= c) || (x /= c && y == c)
  where
    x = password!!(b-1)
    y = password!!(e-1)
    c = head cs

data Range = Range Int Int
  deriving (Show)

instance Read Range where
  readsPrec _ s = readsRange s

data Password = Password Range String String
  deriving (Show)

instance Read Password where
  readsPrec _ s = readsPassword s

readsRange :: ReadS Range
readsRange s = [(Range a b, v) | (a, t) <- reads s
                               , ("-", u) <- lex t
                               , (b, v) <- reads u
                               ]

readsPassword :: ReadS Password
readsPassword s = [(Password r c rest, x) | (r, t) <- readsRange s
                                          , (c, v) <- lex t
                                          , (":", w) <- lex v
                                          , (rest, x) <- lex w
                                          ]
