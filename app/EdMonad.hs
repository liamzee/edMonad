{-# LANGUAGE TypeApplications #-}

module EdMonad where

-- This iteration will assume the filename is always given to the command, and there is no default filename.
-- The !commands part should also be ignored for simplicity and because of the Windows issue

import Data.Char (isDigit)
import Text.Read (readMaybe)
import System.Console.Haskeline
import Control.Monad.IO.Class

-- Oth functions you might need:
-- span
-- break
-- splitAt
-- reverse
-- drop
-- take
-- lines
-- unlines
-- length
-- putStrLn


type Buffer = [String] -- A buffer of multiple lines
type Line = Int        -- A line number
type FileName = String

data Command = Append (Maybe Line)
             | Insert (Maybe Line)
             | Change (Maybe Line) (Maybe Line)
             | Delete (Maybe Line) (Maybe Line)
             | File FileName

data IOCommand = Write FileName
               | Read  FileName
               | Edit  FileName
               | PrLine (Maybe Line) (Maybe Line)
               | Quit

data Mode = CommandMode | InputMode

-- | Parse command processes the given string into either an IOCommand or a
-- Command.
--
-- If the input isn't a valid command, Nothing is returned
parseCommand :: String -> Maybe (Either IOCommand Command)

parseCommand [] = Nothing
parseCommand k = let
 (y,z) = span isAddresses k
 (l,m) = parseLines y
 check = (l,m) == (Nothing,Nothing)
 command = Just . Right
 ioCommand = Just . Left in
  case z of
   "c" -> command $ Change l m
   "d" -> command $ Delete l m
   "p" -> ioCommand $ PrLine l m
   "a" | m == Nothing -> command $ Append l
   "i" | m == Nothing -> command $ Insert l
   ('e':' ':xs) | check -> ioCommand $ Edit xs
   "q" | check -> ioCommand $ Quit
   ('r':' ':xs) | check -> ioCommand $ Read xs
   ('w':' ':xs) | check -> ioCommand $ Write xs
   _ -> Nothing  

-- | Return true if the character might be part of an address range specification
-- i.e. True if is a number or ','
isAddresses :: Char -> Bool
isAddresses x
 | isDigit x || x == ',' = True
 | otherwise = False

-- | Parse a single line number from the whole string
--
-- >>> parseLine 14
-- Just 14
parseLine :: String -> Maybe Line
parseLine [] = Nothing
parseLine x = pure $ read x :: Maybe Line

-- | Parse a line range from the whole string
--
-- >>> parseLines 14,16
-- (Just 14, Just 16)
--
-- >>> parseLines 17
-- (Just 17, Nothing)
parseLines :: String -> (Maybe Line, Maybe Line)
parseLines [] = (Nothing, Nothing)
parseLines k = let (a,b) = break (== ',') k in
  (parseLine a,case b of
    [] -> Nothing
    (c:d) -> parseLine d)
    
-- | Execute a Command on a buffer, at a current line,
-- returning the modified buffer, and the modified line
--
-- Since the command might change the mode, it also returns a modified mode
executeCommand :: Command -> (Buffer, Line) -> (Buffer, Line, Mode)
executeCommand command (buf,ln) = case command of
    Append x -> case x of
        Nothing -> (buf,ln,InputMode)
        Just k -> (buf,k,InputMode)
    Change x y -> case x of
        Nothing -> (dl (ln, Just ln), newLine (ln, Just ln), InputMode)
        Just k -> (dl (k, y), newLine (k, y), InputMode)
    Delete x y -> (\(x,y,z) -> (x,y,CommandMode)) $ executeCommand (Change x y) (buf,ln)
    Insert x -> case x of
        Nothing -> (buf, ln-1,InputMode)
        Just k -> (buf, k-1,InputMode)
    where dl (a,b) = deleteLines (a,b) buf
          newLine (a,b) | length  (dl (a, b)) < a = length $ dl (a, b)
           | otherwise = a

-- | Execute a Command that does IO on a buffer,
-- at a current line, returning the modified
-- buffer, and the modified line.
--
-- IO Commands never change the current mode
executeIOCommand :: IOCommand -> (Buffer, Line) -> InputT IO (Buffer, Line)
executeIOCommand ioCommand (buf,ln) = case ioCommand of
    Edit x -> liftIO $ (\k -> (lines k, length (lines k))) <$> ( (readFile x))
    PrLine x y ->  (\(store,newln) -> outputStr (unlines store) >> pure (buf, newln))
        (case x of
         Nothing -> (getLines (ln, Just ln) buf)
         Just k -> (getLines (k, y) buf))
    Read x ->  liftIO $ (\k -> (inputLines (lines k) (buf,ln))) <$> readFile x
    Write x -> liftIO $ writeFile x (unlines buf) >> pure (buf,ln)

-- | Input line adds the given string to the buffer at the current line
inputLines :: [String] -> (Buffer, Line) -> (Buffer, Line)
inputLines strs (buffer,line)=(take line buffer ++ strs ++ drop line buffer, line+(length strs))

-- | Delete lines from the buffer in the given range. If the right bound of the
-- range is 'Nothing', delete just the specified line, that is, assume (x,x)
deleteLines :: (Line, Maybe Line) -> Buffer -> Buffer
deleteLines (x,y) buffer = case y of
    Nothing -> deleteLines' (x,x) buffer
    Just k -> deleteLines' (x,k) buffer
    where deleteLines' :: (Line, Line) -> Buffer -> Buffer
          deleteLines' (a,b) buffer = take (a-1) buffer ++ drop b buffer

-- | Get a range of lines from a buffer and return the partial buffer (the
-- selected lines) + the line number of the last line of the returned partial
-- buffer
getLines :: (Line, Maybe Line) -> Buffer -> (Buffer, Line)
getLines (x, y) buf = getLines' (x, fromMaybe x y) buf
    where
    getLines' :: (Line, Line) -> Buffer -> (Buffer, Line)
    getLines' (a,b) buf = (take (b-a+1) $ drop (a-1) buf, b)

-- The main function, `ed`, works on a buffer,
-- on a current line, and on a current mode
ed :: (Buffer, Line, Mode) -> InputT IO ()
ed (buffer, line, mode) = do
    userInput <- (fromMaybe "") <$> getInputLine ""
    case mode of
      CommandMode -> case parseCommand userInput of

          Nothing -> ed (buffer, line, mode)

          Just (Left Quit) -> pure ()

          Just (Left ioCommand) -> executeIOCommand ioCommand (buffer, line) >>= (\(x,y) -> ed (x,y,CommandMode))
          
          Just (Right command) -> ed $ executeCommand command (buffer, line)

      InputMode -> if userInput == "."            -- Change to command mode upon "." on a single line
          then ed (buffer, line, CommandMode)
          else ed $ (\(x,y) -> (x,y,InputMode)) $ inputLines [userInput] (buffer, line)

-- | Given a default value and a maybe value of the same type, return the value
-- if it's Just, return the default value if it's Nothing

fromMaybe :: a -> Maybe a -> a
fromMaybe k Nothing = k
fromMaybe k (Just l) = l