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
import System.Exit

import Data.Maybe



-- move cursor but make sure move is not outside board
movePos :: Pos -> Char -> Pos
movePos (r,c) 'w' = if (r > 0) then (r-1,c) else (r,c) 
movePos (r,c) 's' = if (r < 7) then (r+1,c) else (r,c) 
movePos (r,c) 'a' = if (c > 0) then (r,c-1) else (r,c) 
movePos (r,c) 'd' = if (c < 7) then (r,c+1) else (r,c) 
movePos p     _        = p

-- select and un-select piece
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
                            

-- Update Player state: D -> Draw, R -> Resign, P -> Play
updatePlayerState :: Player -> Char -> Player
updatePlayerState (Player n t c st) 'D' = Player n t c Draw -- logic not implemented
updatePlayerState (Player n t c st) 'R' = Player n t c Resign
updatePlayerState (Player n t c st) 'P' = Player n t c Play
updatePlayerState pl _ = pl


-- move game to previous turn
stepbackBoard :: ( MonadState WorldState m) => m ()
stepbackBoard = do 
  player  <- use current
  hist    <- use history
  
  -- first turn
  if (fst $ head hist) == 0 
  then return ()
  else do  
    let newhist = tail $ hist
        prevBoard = snd $ head newhist
    board   .= prevBoard
    history .= newhist
    current %= other


makeTurn :: (MonadIO m, MonadReader Config m, MonadState WorldState m) => m ()
makeTurn = do
  game    <- get
  board   <- use board
  player  <- use current
  pwhite  <- use whitePlayer
  pblack  <- use blackPlayer
 
  liftIO $ showCursor
  liftIO $ hSetEcho stdin False
  liftIO $ threadDelay (1 * 10 ^ 5)
    
  key <- liftIO getChar -- get input
  
  -- option to go back to previous turn
  if key == 'U' 
  then stepbackBoard
  else return ()
 
  
  -- option to change player state: Play, Draw, Resign
  if player == game^.whitePlayer^.col
  then
     whitePlayer .= updatePlayerState (game^.whitePlayer) key
  else
     blackPlayer .= updatePlayerState (game^.blackPlayer) key
  

  let currPos = game^.cursor^.position  -- current Position
      currSel = game^.cursor^.selected  -- current Selection
      --
      newSel  = selectPos currSel newPos key -- update selection
      newPos  = movePos currPos key          -- update position
      --
      possMov = getMoves newSel board
      -- default new cursor
      newcursor = Cursor newPos newSel possMov 
 
 
  if isNothing currSel then 
     selectPiece newSel newcursor
  else 
     selectMove currSel newSel newcursor board   -- when piece is selected          

  
    
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
                        cursor  .= newcursor
                        message .= "piece selected"
                     else 
                        message .= "invalid selection! it is "++show(player)++"'s turn"                             

selectMove :: (MonadState WorldState m) => Maybe Pos -> Maybe Pos -> Cursor -> Board -> m ()
selectMove currSel newSel newcursor board
  | isNothing newSel     = do cursor .= newcursor >> message .= "select piece"
  | currSel == newSel    = do cursor .= newcursor >> message .= "select move"
  | elem justSel possMov = makemove justSel --make move if is in possible moves
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
    -- update board (for Enpassant: change Pawns in TwoStep State -> Moved) 
    board  .= setTwoStepMoved (other player) newBoard 
    cursor .= Cursor to Nothing []  -- reset cursor sel
    current %= other                -- change turn
    history .= ((fst $ head hist) + 1,newBoard):hist
  
    -- did the move put new player in check?
    if isCheck (other player) newBoard then
      message .= "Atention! "++show(other player)++" is in Check!"
    else
      message .= "" 
  
  

startGame :: Config -> IO WorldState
startGame conf = do
  clearScreen  

  let (mrow,mcol) = screenSize conf                  -- terminal bounds 
  liftIO $ forM_ (zip (repeat 0) [0 .. mcol]) (renderPoint '─')
  liftIO $ setCursorPosition 0 ((div mcol 2)-18)
  liftIO $ putStrLn " Haskell Terminal Chess Game "
  
  putStrLn ""
  setSGR [SetColor Foreground Vivid System.Console.ANSI.White]
  putStrLn "Enter white player name: "  
  p1name <- getLine
  setSGR [SetColor Foreground Vivid System.Console.ANSI.Black]
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
  pwhite <- use whitePlayer
  pblack <- use blackPlayer
  
  if isCheckMate player board || pwhite^.stat == Resign || pblack^.stat == Resign
  then do 
     liftIO $ threadDelay (2 * 10 ^ 6)
     renderGameOver
    
     liftIO $ threadDelay (3 * 10 ^ 6) 
     lift $ lift $ exitSuccess
  else do 
     renderGame
     makeTurn 


  
main :: IO ()
main = do
  noBuffering
  conf <- mkConfig
  game <- startGame conf
  runStateT (runReaderT play conf) game
  
  return ()
  


