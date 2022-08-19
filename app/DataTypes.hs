{-# LANGUAGE TemplateHaskell #-}
module DataTypes where

import Control.Lens

data Config = Config { screenSize :: (Int,Int) }

data PColor = White | Black deriving (Show,Eq)
data PState = Init | TwoStep | Moved deriving (Eq,Show)

data PType = King | Rook | Queen | Bishop | Knight | Pawn deriving (Eq,Show)
data Piece = Piece { _ptype  :: PType
                    ,_pcolor :: PColor
                    ,_pstate :: PState} deriving (Show,Eq)
makeLenses ''Piece                    

type Pos = (Int,Int) -- (row, col)

type Board = [Square]
data Square = Square {_pos  ::Pos
                     ,_piece::Piece} deriving (Show,Eq)
makeLenses ''Square  


data Cursor = Cursor { _position :: Pos
                     , _selected :: Maybe Pos
                     , _possMove :: [Pos]} deriving (Show,Eq)
makeLenses ''Cursor


data State = Draw | Resign | Play deriving (Show,Eq) 

data Player = Player { _name :: String
                     , _time :: Float
                     , _col  :: PColor
                     , _stat :: State} deriving (Show,Eq)
makeLenses ''Player
                       
data WorldState = WorldState { _board  :: Board
                             , _cursor :: Cursor
                             , _whitePlayer :: Player
                             , _blackPlayer :: Player
                             , _current :: PColor
                             , _message :: String
                             , _history :: [(Int, Board)] } deriving Show
makeLenses ''WorldState

