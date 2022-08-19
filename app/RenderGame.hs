{-# LANGUAGE FlexibleContexts #-}

module RenderGame where

import DataTypes
import GameLogic

import Control.Monad.Reader
import Control.Monad.State 
import System.Console.ANSI
import Control.Lens

import Data.Maybe
import Data.Either


--renderGame :: ReaderT Config (StateT Game IO) ()
renderGame :: (MonadIO m, MonadReader Config m, MonadState WorldState m) => m ()
renderGame = do
  game <- get
  conf <- ask
  let (mrow,mcol) = screenSize conf                  -- terminal bounds   
      (brow,bcol) = ((div mrow 2)-6,(div mcol 2)-12) -- board start point

  liftIO $ hideCursor
--  liftIO $ setSGR [Reset]
  -- Header
  liftIO $ clearScreen
  liftIO $ forM_ (zip (repeat 0) [0 .. mcol]) (renderPoint '-')
  liftIO $ setCursorPosition 0 ((div mcol 2)-18)
  liftIO $ putStrLn " Haskell Terminal Chess Game "

  
  liftIO $ setCursorPosition 1 0
  liftIO $ putStr $ "White Player: " ++ (game ^. whitePlayer ^. name )
  liftIO $ setCursorPosition 2 0
  liftIO $ putStrLn $ "Black Player: " ++ (game ^. blackPlayer ^. name) 
  
  liftIO $ setCursorPosition 1 (mcol -30)
  liftIO $ putStr $ "Current Player: "++ show (game ^. current) ++ " player"
  liftIO $ setCursorPosition 2 (mcol -30)
  liftIO $ putStr $ "Turn: "++ show (fst$  head $game^.history) 

  

  
  
  --
  -- Board Bounds  
  liftIO $ renderLine " a b c d e f g h" (brow-2,bcol-1)
  liftIO $ renderLine "╔═════════════════╗"  (brow-1,bcol-2)
  
  liftIO $ renderPoint '1' (brow,bcol-4)
  liftIO $ renderPoint '2' (brow+1,bcol-4)
  liftIO $ renderPoint '3' (brow+2,bcol-4)
  liftIO $ renderPoint '4' (brow+3,bcol-4)
  liftIO $ renderPoint '5' (brow+4,bcol-4)
  liftIO $ renderPoint '6' (brow+5,bcol-4)
  liftIO $ renderPoint '7' (brow+6,bcol-4)
  liftIO $ renderPoint '8' (brow+7,bcol-4)
  liftIO $ forM_ (zip [brow .. brow+7] (repeat $ bcol-2)) (renderLine "║|")

  liftIO $ renderPoint '1' (brow,bcol+18)
  liftIO $ renderPoint '2' (brow+1,bcol+18)
  liftIO $ renderPoint '3' (brow+2,bcol+18)
  liftIO $ renderPoint '4' (brow+3,bcol+18)
  liftIO $ renderPoint '5' (brow+4,bcol+18)
  liftIO $ renderPoint '6' (brow+5,bcol+18)
  liftIO $ renderPoint '7' (brow+6,bcol+18)
  liftIO $ renderPoint '8' (brow+7,bcol+18)
  liftIO $ forM_ (zip [brow .. brow+7] (repeat $ bcol+16)) (renderPoint '║')
  
  liftIO $ renderLine "╚═════════════════╝"  (brow+8,bcol-2)  
  liftIO $ renderLine " a b c d e f g h" (brow+9,bcol-1)


  -- Print instructions
  let rInst = div (3*mrow) 4
  liftIO $ renderLine ("─────── Commands ────────────") (rInst, 0)
  liftIO $ renderLine ("  'w'") (rInst+1, 0)
  liftIO $ renderLine ("'a s d'       -> Move") (rInst+2, 0)
  liftIO $ renderLine ("Enter/Space   -> Select") (rInst+3, 0)
  liftIO $ renderLine ("'q'/ESC       -> un-select") (rInst+4, 0) 
  liftIO $ renderLine ("'R'           -> Resign") (rInst+5, 0)
  liftIO $ renderLine ("'U'           -> Undo") (rInst+6, 0)  
  --
  -- Print Board 
  board <- use board -- get board
  player  <- use current


-- Message, selected piece and cursor piece
  let posPStr = case getPiece newPos board of 
               Left  m  -> ""
               Right p  -> (" ("++show(p^.ptype)++" "++show(p^.pcolor)++")")
      selPStr = case getPiece selPos board of 
               Left  m  -> ""
               Right p  -> (show(p^.ptype)++" "++show(p^.pcolor))      
              
      spm = if isEnpassant pieceSel selPos newPos board --
              then ". EnPassant! Pawn's Special Move"
            else if isCastling pieceSel selPos newPos board
                   then ". Castling! King's Special move"
                   else ""  
      pieceSel = fromRight (Piece King DataTypes.White TwoStep) $getPiece (selPos) board
      newPos  = game^.cursor^.position     
      selPos  = case game ^. cursor ^. selected of   
                    Nothing  -> ((-1),(-1)) -- pos out of board bounds
                    Just pos -> pos         -- return pos                
                    
  -- Message
  liftIO $ renderLine ("info:     "++ game^.message ++ posPStr) (brow+11, bcol-5) 
  liftIO $ renderLine ("selected: "++selPStr++ spm) (brow+12, bcol-5) 
--  liftIO $ renderLine ("cursor:   "++posPStr) (brow+12, bcol-5)                  
  
  
  let -- render board with colors 
      renderBoard (r,c) 
         | (r,c) == selPos             = do setSGR [SetColor Background Dull Blue] >> render                
         | (r,c) == kingPos && inCheck = do setSGR [SetColor Background Dull Red] >> render
         | elem (r,c) possMov          = do setSGR [SetColor Background Vivid Blue] >> render
         | otherwise                   = do setSGR [Reset] >> render
              where render    = renderLine (getChessChar (r,c) board) (brow+r,bcol+c*2)
                    kingPos   = getKingPos player board
                    inCheck = isCheck player board
                    possMov = game ^. cursor ^. possMove
      
      renderGap (r,c) = do setSGR [Reset] >> renderPoint '|' (brow+r,bcol+c*2+1)
      -- all board positions                            
      allpos    = [(x,y) | x <- [0..7], y <- [0..7]]                 
 
  liftIO $ mapM_ renderBoard allpos   
  
  liftIO $ mapM_ renderGap allpos   


  
  -- show cursor in position
  let (rc,cc) = game^.cursor^.position
  liftIO $ setCursorPosition (brow+rc) (bcol+cc*2)

  
  return ()

renderGameOver :: (MonadIO m, MonadReader Config m, MonadState WorldState m) => m ()
renderGameOver = do
  game   <- get 
  conf   <- ask
  player <- fmap other $ use current
  pwhite <- use whitePlayer
  pblack <- use blackPlayer
  
  let (mrow,mcol) = screenSize conf                  -- terminal bounds   
  
  liftIO $ hideCursor
  liftIO $ clearScreen
  
  liftIO  $ renderLine ("GAME OVER! "++show(player)++" WINS!") (div mrow 2,(div mcol 2)-6)


  if player == pwhite^.col
  then liftIO  $ renderLine ("Congratulations "++pwhite^.name++"!") ((div mrow 2)+2,(div mcol 2)-6)
  else liftIO  $ renderLine ("Congratulations "++pblack^.name++"!") ((div mrow 2)+2,(div mcol 2)-6)
  
  liftIO $ setCursorPosition (mrow) (0)
  liftIO $ showCursor

renderPoint :: Char -> (Int,Int) -> IO ()
renderPoint c (row,col) = do
     setCursorPosition row col >> putChar c
     
renderLine :: String -> (Int,Int) -> IO ()
renderLine s (row,col) = do
     setCursorPosition row col >> putStr s




-------------------------------------------------
---- FUNCTION TO DRAW BOARD -- used to debug GameLogic
drawBoard :: Board -> IO ()
drawBoard b = do
   putStrLn   ""
   putStrLn $ "   " ++ " a b c d e f g h" 
   putStrLn $ "   " ++ "-----------------" 
   putStrLn $ "1 ║ " ++ concat [getChessChar (0,y) b | y <- [0..7]] ++ "║ 1"
   putStrLn $ "2 ║ " ++ concat [getChessChar (1,y) b | y <- [0..7]] ++ "║ 2"
   putStrLn $ "3 ║ " ++ concat [getChessChar (2,y) b | y <- [0..7]] ++ "║ 3"
   putStrLn $ "4 ║ " ++ concat [getChessChar (3,y) b | y <- [0..7]] ++ "║ 4"
   putStrLn $ "5 ║ " ++ concat [getChessChar (4,y) b | y <- [0..7]] ++ "║ 5"
   putStrLn $ "6 ║ " ++ concat [getChessChar (5,y) b | y <- [0..7]] ++ "║ 6"
   putStrLn $ "7 ║ " ++ concat [getChessChar (6,y) b | y <- [0..7]] ++ "║ 7"
   putStrLn $ "8 ║ " ++ concat [getChessChar (7,y) b | y <- [0..7]] ++ "║ 8"
   putStrLn $ "   " ++ "-----------------" 
   putStrLn $ "   " ++ " a b c d e f g h" 

getChessChar :: Pos -> Board -> String
getChessChar pos b = let
  symbol :: Piece -> Char
  symbol (Piece typ col sta)
     | typ == Rook     = case col of
                            DataTypes.White  -> '♜' 
                            DataTypes.Black  -> '♖'
     | typ == Bishop   =  case col of
                            DataTypes.White  -> '♝' 
                            DataTypes.Black  -> '♗'
     | typ == Knight   =   case col of
                            DataTypes.White  -> '♞' 
                            DataTypes.Black  -> '♘'
     | typ == Queen    =  case col of
                            DataTypes.White  -> '♛' 
                            DataTypes.Black  -> '♕'
     | typ == King     =  case col of
                            DataTypes.White  -> '♚' 
                            DataTypes.Black  -> '♔'
     | typ == Pawn     =  case col of
                            DataTypes.White  -> '♟' 
                            DataTypes.Black  -> '♙'
  in case getPiece pos b of
       Left m   -> "."
       Right p  ->  symbol p:[]
