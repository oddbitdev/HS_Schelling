{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}

module Main where

import           Data.Maybe                   (fromMaybe)
import           Diagrams.Backend.SVG.CmdLine
import           Diagrams.Prelude
import           Schelling


world = Schelling.createRandomWorld 32 32

tile = Schelling.Tile Schelling.EmptyT (Schelling.TilePosition 0 0)

getCol :: Int -> Int -> [Tile] -> Colour Double
getCol x y ts = case tt of
                    Schelling.EmptyT -> white
                    Schelling.WhiteT -> green
                    Schelling.BlackT -> blue
    where tp  = Schelling.TilePosition x y
          mtt = fromMaybe tile (Schelling.tileAtPos ts tp)
          tt  = Schelling.tileType mtt


example :: IO (Diagram B)
example = do w <- world
             let tls = tiles w
             let sqrs = [[ square 1 # fc (getCol x y tls) # lw none | x <- [0..31]] | y <- [0..31]]
             let h = map hcat sqrs
             let v = vcat h
             return v

main = mainWith example
