module Day3
    ( day3a
    , day3b
    ) where

day3a :: String -> Int
day3a = countGeo 3 1 . read

day3b :: String -> Int
day3b str =
  countGeo 1 1 geo *
  countGeo 3 1 geo *
  countGeo 5 1 geo *
  countGeo 7 1 geo *
  countGeo 1 2 geo
  where geo = read str

countGeo :: Int -> Int -> Geo -> Int
countGeo dx dy = sum . map (countNode . head) . dropX dx . map cycle . dropY dy . unwrap

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
      if c `mod` dy == 0
        then x : go xs (c + 1)
        else go xs (c + 1)

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

