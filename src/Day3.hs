module Day3
    ( day3a
    ) where

day3a :: String -> Geo
day3a = read

data MapNode = WithTree | WithoutTree

instance Read MapNode where
  readsPrec _ s =
    [(WithTree, t) | ("#", t) <- lex s] ++
    [(WithoutTree, t) | (".", t) <- lex s]

instance Show MapNode where
  show WithTree    = "#"
  show WithoutTree = "."

newtype GeoCol = GeoCol [MapNode]

instance Read GeoCol where
  readsPrec _ [] =
    [(GeoCol [], "")]
  readsPrec _ (x:xs)  =
    [(GeoCol (n:ns), z) | (n, _) <- reads [x] -- should I use ReadP instead?
                        , (GeoCol ns, z) <- reads xs]

instance Show GeoCol where
  show (GeoCol xs) = concatMap show xs

newtype Geo = Geo [GeoCol]

instance Read Geo where
  readsPrec _ s = [(Geo (map read ls), "")]
    where ls = lines s

instance Show Geo where
  show (Geo [x]) = show x
  show (Geo (x:xs)) = show x ++ "\n" ++ show (Geo xs)

