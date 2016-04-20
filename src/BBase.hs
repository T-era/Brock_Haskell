module BBase(Pos(..)) where

newtype Pos d = Pos (d, d) deriving (Show, Eq, Ord)
