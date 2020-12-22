module Day3
    ( day3a
    ) where

day3a :: String -> Int
day3a = countGeo 3 1 . read

countGeo :: Int -> Int -> Geo -> Int
countGeo dx dy = sum . map countNode . dropY dy . map head . dropX dx . map cycle . unwrap

unwrap :: Geo -> [[MapNode]]
unwrap (Geo xs) = map unwrapCol xs
  where unwrapCol (GeoCol ys) = ys

dropX :: Int -> [[a]] -> [[a]]
dropX dx xss = go xss 0
  where
    go [] _ = []
    go (ys:yss) c = drop c ys : go yss (c + dx)

dropY :: Int -> [a] -> [a]
dropY dy xss = go xss 0
  where
    go [] _ = []
    go (x:xs) c =
      if c `mod` dy /= 0
        then go xs (c + 1)
        else x : go xs (c + 1)

countNode :: MapNode -> Int
countNode WithTree    = 1
countNode WithoutTree = 0

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

