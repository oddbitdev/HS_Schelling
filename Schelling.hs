module Schelling where


import           Data.List.Unique             (repeated)
import           Data.Maybe                   (fromMaybe, isJust)
import           GHC.Exts                     (groupWith)
import           Math.Geometry.Grid
import           Math.Geometry.Grid.Octagonal
import           System.Random


data TileType = EmptyT | WhiteT | BlackT deriving (Show, Bounded, Enum, Eq, Ord)

instance Random TileType where
      random g = case randomR (fromEnum (minBound :: TileType), fromEnum (maxBound :: TileType)) g of
                   (r, g') -> (toEnum r, g')
      randomR (a,b) g = case randomR (fromEnum a, fromEnum b) g of
                          (r, g') -> (toEnum r, g')

data TilePosition = TilePosition { column :: Int
                                 , row    :: Int }
                                 deriving (Show, Eq, Ord)
data Tile = Tile { tileType     :: TileType
                 , tilePosition :: TilePosition }
                 deriving (Show, Eq, Ord)

data World = World { tiles     :: [Tile]
                   , structure :: RectOctGrid }
                   deriving Show

tileAtPos :: [Tile] -> TilePosition -> Maybe Tile
tileAtPos g tp = case g of
      [] -> Nothing
      (x:xs) | tilePosition x == tp -> Just x
             | otherwise            -> tileAtPos xs tp

tileNeighbours :: World -> Tile -> [Tile]
tileNeighbours w t = filter
      (\a -> (column (tilePosition a), row (tilePosition a)) `elem` ns)
      (tiles w)
   where
      ns = neighbours (structure w) (column tp, row tp)
      tp = tilePosition t

tileDesirability :: World -> Tile -> TileType -> Bool -> (Tile, Double)
tileDesirability w t tt is = (t, (2 * same  - 2 * diff + empty) / (2 * ns))
   where
      s     = fromIntegral $ length $ filter (\a -> tileType a == tt) n
      same  = if is then s else s - 1
      diff  = fromIntegral $ length $ filter
              (\a -> tileType a /= tt && tileType a /= EmptyT)
              n
      e     = fromIntegral $ length $ filter (\a -> tileType a == EmptyT) n
      empty = if is then e else e + 1
      n     = tileNeighbours w t
      ns    = fromIntegral $ length n

desirabilityMatrix :: World -> Tile -> [(Tile, Double)]
desirabilityMatrix w t = desMap
   where
      ns      = tileNeighbours w t
      emptyNs = filter (\a -> tileType a == EmptyT) ns :: [Tile]
      desMap  = [ tileDesirability w nt (tileType t) False | nt <- emptyNs ]

wantsToMoveToTile :: World -> Tile -> Maybe Tile
wantsToMoveToTile w t = if bestTile == t then Nothing else Just bestTile
   where
      desMatrix = desirabilityMatrix w t
      bestTile  = fst
            ( foldr (\a b -> if snd a > snd b then a else b)
                    (tileDesirability w t (tileType t) True)
                    desMatrix
            )

movementMatrix :: World -> [(Tile, Tile)]
movementMatrix w = finalMM
   where
      worldTiles = tiles w
      worldMM =
            [ (t, wantsToMoveToTile w t)
            | t <- worldTiles
            , tileType t /= EmptyT
            ]
      nonNullMM   = filter (\a -> isJust (snd a)) worldMM
      extractedMM = [ (a, fromMaybe a b) | (a, b) <- nonNullMM ]
      toTiles     = [ snd tt | tt <- extractedMM ]
      dupsToTiles = repeated toTiles
      dupsTT      = filter (\a -> elem (snd a) dupsToTiles) extractedMM
      groupedDups = groupWith (\a -> snd a) dupsTT
      uniqueTT    = filter (\a -> notElem (snd a) dupsToTiles) extractedMM
      nonDups     = filterLowestDesTile w groupedDups
      finalMM     = uniqueTT ++ nonDups

filterLowestDesTile :: World -> [[(Tile, Tile)]] -> [(Tile, Tile)]
filterLowestDesTile w xs = m''
   where m   = [ [ ((tileDesirability w t (tileType t) True), t') | (t, t') <- i ] | i <- xs ]
         m'  = map (\a -> foldl (\b c -> if (snd (fst b)) < (snd (fst c)) then b else c) (head a) a) m
         m'' = [ (fst (fst t), snd t) | t <- m' ]

worldStep :: World -> World
worldStep w = newWorld
   where
      worldMM = movementMatrix w
      flatMM  = map fst worldMM ++ map snd worldMM
      switchedMM =
            [ ( Tile (tileType (snd a)) (tilePosition (fst a))
              , Tile (tileType (fst a)) (tilePosition (snd a))
              )
            | a <- worldMM
            ]
      flatSwitchedMM = map fst switchedMM ++ map snd switchedMM
      nonMovingTiles = [ t | t <- (tiles w), notElem t flatMM ]
      finalTiles     = nonMovingTiles ++ flatSwitchedMM
      newWorld       = World finalTiles (structure w)

createRandomWorld :: Int -> Int -> IO World
createRandomWorld cols rows = do g <- newStdGen
                                 let worldStructure = rectOctGrid cols rows
                                 let tilePositions  = [uncurry TilePosition tp | tp <- (indices worldStructure)]
                                 let tileTypes      = take (cols * rows) (randoms g :: [TileType])
                                 let worldTiles     = zipWith Tile tileTypes tilePositions
                                 let world          = World worldTiles worldStructure
                                 return world


main :: IO ()
main = do world <- createRandomWorld 32 32
          let t = tileAtPos (tiles world) (TilePosition 0 0)
          case t of
                Just a -> do
                      print (tileDesirability world a (tileType a) True)
                      print (desirabilityMatrix world a)
                Nothing -> putStrLn "Nothing"
          let nw = worldStep world
          print (length (tiles nw))
          print (tileAtPos (tiles world) (TilePosition 16 16))
          print (tileAtPos (tiles nw) (TilePosition 16 16))
          putStrLn "End"



