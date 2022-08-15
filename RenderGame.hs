{-# LANGUAGE FlexibleContexts #-}

module RenderGame where

import DataTypes
import GameLogic

import Control.Monad.Reader
import Control.Monad.State 
import System.Console.ANSI
import Control.Lens


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
  liftIO $ setCursorPosition 1 (mcol -27)
  liftIO $ putStr $ "Current Turn: "++ show (game ^. current) ++ " player"
  --
  -- Board Bounds  
  liftIO $ renderLine " a b c d e f g h" (brow-2,bcol-1)
  liftIO $ renderLine "-----------------"  (brow-1,bcol-1)
  
  liftIO $ renderPoint '1' (brow,bcol-4)
  liftIO $ renderPoint '2' (brow+1,bcol-4)
  liftIO $ renderPoint '3' (brow+2,bcol-4)
  liftIO $ renderPoint '4' (brow+3,bcol-4)
  liftIO $ renderPoint '5' (brow+4,bcol-4)
  liftIO $ renderPoint '6' (brow+5,bcol-4)
  liftIO $ renderPoint '7' (brow+6,bcol-4)
  liftIO $ renderPoint '8' (brow+7,bcol-4)
  liftIO $ forM_ (zip [brow .. brow+7] (repeat $ bcol-2)) (renderPoint '|')

  liftIO $ renderPoint '1' (brow,bcol+18)
  liftIO $ renderPoint '2' (brow+1,bcol+18)
  liftIO $ renderPoint '3' (brow+2,bcol+18)
  liftIO $ renderPoint '4' (brow+3,bcol+18)
  liftIO $ renderPoint '5' (brow+4,bcol+18)
  liftIO $ renderPoint '6' (brow+5,bcol+18)
  liftIO $ renderPoint '7' (brow+6,bcol+18)
  liftIO $ renderPoint '8' (brow+7,bcol+18)
  liftIO $ forM_ (zip [brow .. brow+7] (repeat $ bcol+16)) (renderPoint '|')
  
  liftIO $ renderLine "-----------------"  (brow+8,bcol-1)  
  liftIO $ renderLine " a b c d e f g h" (brow+9,bcol-1)


  -- Message
  liftIO $ renderLine ("info: "++ game ^. message) (brow+11, bcol - 5)
  
  -- Print instructions
  liftIO $ renderLine ("------- Commands -----------") (brow+13, 0)
  liftIO $ renderLine ("  'w'") (brow+14, 0)
  liftIO $ renderLine ("'a s d'          -> to move cursor") (brow+15, 0)
  liftIO $ renderLine ("Enter or Space   -> select") (brow+16, 0)
  liftIO $ renderLine ("ESC or 'q'       -> un-select") (brow+17, 0) 
  --
  -- Print Board 
  board <- use board -- get board
  curr  <- use current
  
  let -- render board with colors 
      renderBoard (r,c) 
         | (r,c) == selPos             = do setSGR [SetColor Background Dull Blue] >> render                 
         | (r,c) == kingPos && inCheck = do setSGR [SetColor Background Dull Red] >> render
         | elem (r,c) possMov          = do setSGR [SetColor Background Vivid Blue] >> render
         | otherwise                   = do setSGR [Reset] >> render
              where render    = renderLine (getChessChar (r,c) board) (brow+r,bcol+c*2)
                    kingPos   = getKingPos curr board
                    inCheck = isCheck curr board
                    possMov = game ^. cursor ^. possMove
                    -- get position selected
                    selPos  = case game ^. cursor ^. selected of   
                                 Nothing  -> ((-1),(-1)) -- pos out of board bounds
                                 Just pos -> pos         -- return pos 
      -- all board positions                            
      allpos    = [(x,y) | x <- [0..7], y <- [0..7]]                 
 
  liftIO $ mapM_ renderBoard allpos 
  
    -- cursor
  let (rc,cc) = game ^. cursor ^. position
  liftIO $ setCursorPosition (brow+rc) (bcol+cc*2)

  
  return ()

-- mapM_ runs a function for each eelement of a list
-- forM_ does the same with inverter arguments


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
   putStrLn $ "1 | " ++ concat [getChessChar (0,y) b | y <- [0..7]] ++ "| 1"
   putStrLn $ "2 | " ++ concat [getChessChar (1,y) b | y <- [0..7]] ++ "| 2"
   putStrLn $ "3 | " ++ concat [getChessChar (2,y) b | y <- [0..7]] ++ "| 3"
   putStrLn $ "4 | " ++ concat [getChessChar (3,y) b | y <- [0..7]] ++ "| 4"
   putStrLn $ "5 | " ++ concat [getChessChar (4,y) b | y <- [0..7]] ++ "| 5"
   putStrLn $ "6 | " ++ concat [getChessChar (5,y) b | y <- [0..7]] ++ "| 6"
   putStrLn $ "7 | " ++ concat [getChessChar (6,y) b | y <- [0..7]] ++ "| 7"
   putStrLn $ "8 | " ++ concat [getChessChar (7,y) b | y <- [0..7]] ++ "| 8"
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
       Left m   -> ". "
       Right p  ->  symbol p:" "
