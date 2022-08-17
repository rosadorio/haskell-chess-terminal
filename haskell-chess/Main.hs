{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

import DataTypes
import GameLogic
import RenderGame

import System.IO
import Control.Monad.Reader
import Control.Monad.State 
import System.Console.ANSI
import Control.Concurrent
import Control.Lens

import Data.Maybe



-- move cursor but make sure move is not outside board
movePos :: Pos -> Char -> Pos
movePos (r,c) 'w' = if (r > 0) then (r-1,c) else (r,c) 
movePos (r,c) 's' = if (r < 7) then (r+1,c) else (r,c) 
movePos (r,c) 'a' = if (c > 0) then (r,c-1) else (r,c) 
movePos (r,c) 'd' = if (c < 7) then (r,c+1) else (r,c) 
movePos p     _        = p


selectPos :: Maybe Pos -> Pos -> Char -> Maybe Pos
selectPos _  _   '\ESC'  = Nothing
selectPos _  _   'q'     = Nothing
selectPos _  pos '\n'    = Just pos
selectPos _  pos ' '     = Just pos
selectPos sel _  _       = sel

getMoves :: Maybe Pos -> Board -> [Pos]
getMoves Nothing    _  = []
getMoves (Just pos) b  = case getPiece pos b of
                            Left m  -> []
                            Right p -> getPossibleMoves p pos b


makeTurn :: (MonadIO m, MonadReader Config m, MonadState WorldState m) => m ()
makeTurn = do
  game    <- get
  board   <- use board
  player <- use current
  
  liftIO $ showCursor
  liftIO $ hSetEcho stdin False
  liftIO $ threadDelay (2 * 10 ^ 5)
    
  key <- liftIO getChar -- get input
  
  let currPos = game^.cursor^.position
      currSel = game^.cursor^.selected
      --
      newSel  = selectPos currSel newPos key
      newPos  = movePos currPos key
      --
      possMov = getMoves newSel board
      -- default new cursor
      newcursor = Cursor newPos newSel possMov
 
  -- when nothing is selected
  if isNothing currSel then
    selectPiece newSel newcursor
  -- when piece is selected          
  else 
    selectMove currSel newSel newcursor board
    
    
    
selectPiece :: (MonadState WorldState m) => Maybe Pos -> Cursor -> m ()
selectPiece newSel newcursor = do
  board   <- use board
  player  <- use current
  
  -- if no new selection 
  if isNothing newSel then do
    cursor  .= newcursor
    message .= "select piece"
  -- if new selection
  else case getPiece (fromJust newSel) board of 
          Left m  -> message .= "invalid selection! "++m 
                  -- if color of piece matches current turn 
          Right p -> if _pcolor p == player then do
                        let piece = show(_pcolor p) ++" "++ show(_ptype p) 
                        cursor  .= newcursor
                        message .= piece ++" selected"
                     else 
                        message .= "invalid selection! it is "++show(player)++"'s turn"                             

selectMove :: (MonadState WorldState m) => Maybe Pos -> Maybe Pos -> Cursor -> Board -> m ()
selectMove currSel newSel newcursor board
  | isNothing newSel     = do cursor .= newcursor >> message .= "select piece"
  | currSel == newSel    = do cursor .= newcursor >> message .= "make move"
  | elem justSel possMov = makemove justSel
  | otherwise            = do cursor .= Cursor justSel Nothing [] >> message .= "invalid move"
    where justSel = fromJust newSel
          possMov = getMoves currSel board 
-- getMoves from current selection (piece postion) instead of new selection (piece destination)



makemove :: (MonadState WorldState m) => Pos -> m ()
makemove to = do
  game   <- get
  player <- use current
  b      <- use board
  hist   <- use history
  
  let from = fromJust (game^.cursor^.selected) 
      newBoard = movePiece from to b  -- get new board after move
  
  
  if isCheck player newBoard then
    message .= "invalid move! You are putting your King in Check-Mate"
  else do
    -- update board (with change Pawns in TwoStep State -> Moved) 
    board  .= setTwoStepMoved (other player) newBoard 
    cursor .= Cursor to Nothing []  -- reset cursodr
    current %= other                -- change turn
    history .= ((fst $ head hist) + 1,newBoard):hist
  
  -- change every current (color) piece state from TwoState -> Moved before changing turn
      -- did the move put new player in check?
    if isCheck (other player) newBoard then
      --message .= "Atention! "++show(other player)++" is in Check!"
      if isCheckMate (other player) newBoard then
         message .= "You are DEAD!"
      else
         message .= "Atention! "++show(other player)++" is in Check!"
    else
      message .= "" 



startGame :: IO WorldState
startGame = do
  clearScreen  
  
  setSGR [SetColor Background Dull Yellow]
  putStrLn "\n----- Welcome to Haskell Chess Game -----"
  putStrLn ""
  setSGR [SetColor Foreground Vivid System.Console.ANSI.White]
  putStrLn "Enter white player name: "  
  p1name <- getLine
  setSGR [SetColor Foreground Dull System.Console.ANSI.Black]
  putStrLn "Enter black player name: "  
  p2name <- getLine
  setSGR [Reset]  -- reset to default colour scheme
  
  
  let timePerTurn = 5  
  return (initGame p1name p2name timePerTurn)


mkConfig :: IO Config
mkConfig = do
  Just (row,col) <- getTerminalSize
  return $ Config { screenSize = (row,col) }


noBuffering :: IO ()
noBuffering = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering


play :: ReaderT Config (StateT WorldState IO) ()
play = forever $ do
  game   <- get
  board  <- use board
  player <- use current
  
  if isCheckMate player board
  then do 
     renderGameOver
  else do 
     renderGame
     makeTurn 
  

  return ()


  
main :: IO ()
main = do
  noBuffering
  game <- startGame
  conf <- mkConfig
  runStateT (runReaderT play conf) game
  
    
  _ <- getChar

  
  setSGR [SetColor Foreground Dull Blue]
  setSGR [SetUnderlining  SingleUnderline]
  putStr "Enter your name: "
  setSGR [SetColor Foreground Dull Yellow]
  hFlush stdout  -- flush the output buffer before getLine
  name <- getLine
  setSGR [SetColor Foreground Dull Blue]
  putStrLn $ "Hello, " ++ name ++ "!"
  setSGR [Reset]  -- reset to default colour scheme
  
  


