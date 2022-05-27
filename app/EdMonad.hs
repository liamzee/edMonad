{-# LANGUAGE TypeApplications #-}

module EdMonad where

-- This iteration will assume the filename is always given to the command, and there is no default filename.
-- The !commands part should also be ignored for simplicity and because of the Windows issue

import Data.Char (isDigit)
import Text.Read (readMaybe)
import System.Console.Haskeline
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.State.Class
import Control.Monad.Identity (Identity)
import Control.Applicative ((<**>))
import Data.Functor.Identity (runIdentity)

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


data Ed m a = Ed {runEd :: (Buffer,Line) -> m (a, (Buffer, Line))}

instance Functor m => Functor (Ed m) where
    fmap f (Ed a) = Ed $ \k -> fmap (\(a, s) -> (f a, s)) (a k)

instance Monad m => Applicative (Ed m) where
    pure = lift.pure
    (Ed a) <*> (Ed b) = Ed $ \s -> do
        (o,s') <- a s
        (o',s'') <- b s'
        pure (o o', s'')

instance Monad m => Monad (Ed m) where
    (Ed a) >>= f = Ed $ \s -> do
        (o,s') <- a s
        runEd (f o) s'

instance MonadTrans Ed where
    lift m = Ed $ \s -> do
        a <- m
        pure (a,s)

instance Monad m => MonadState (Buffer,Line) (Ed m) where
    get = Ed $ \s -> pure (s,s)
    put = \s -> Ed $ \v -> pure ((),s)

instance MonadIO m => MonadIO (Ed m) where
    liftIO = lift.liftIO



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
--executeCommand :: Command -> (Buffer, Line) -> (Buffer, Line, Mode)

executeCommand :: (Applicative m) => Command -> Ed m Mode
executeCommand command = case command of
    Delete x y -> (const CommandMode) <$> (executeCommand (Change x y))
    _ -> Ed $ \(buf,ln) -> pure $ case command of
        Append x -> (InputMode,(buf,fromMaybe ln x))
        Change x y -> case x of
            Nothing -> (InputMode, (dl (ln, Just ln) (buf,ln), newLine (ln, Just ln) (buf,ln)))
            Just k -> (InputMode, (dl (k, y) (buf,ln), newLine (k, y) (buf,ln)))
        Insert x -> (InputMode,(buf, (fromMaybe (ln) x) - 1 ) )
    where dl (a,b) (buf,ln) = (\(_,(a,b)) -> a ) $ runIdentity $ runEd (deleteLines (a,b)) (buf,ln)
          newLine (a,b) (buf,ln) | length (dl (a, b) (buf,ln)) < a = length $ dl (a, b) (buf,ln)
           | otherwise = a

-- | Execute a Command that does IO on a buffer,
-- at a current line, returning the modified
-- buffer, and the modified line.
--
-- IO Commands never change the current mode
--executeIOCommand :: IOCommand -> (Buffer, Line) -> InputT IO (Buffer, Line)

executeIOCommand :: IOCommand -> Ed (InputT IO) ()
executeIOCommand ioCommand = case ioCommand of
    Read x ->  (liftIO $ readFile x) >>= inputLines.lines
    _ -> Ed $ \(buf,ln) -> case ioCommand of
        Edit x -> do
            u <- liftIO $ readFile x
            pure ((),(lines u, length $ lines u) )
        PrLine x y -> 
            (\(store,newln) -> do
            outputStr (unlines store)
            pure ((), (buf, newln) ) )
            $  (case x of
                    Nothing -> (runEd (getLines (ln, Just ln)) (buf,ln))
                    Just k -> (runEd (getLines (k, y))) (buf,ln))
                            >>= (\(_,k) -> k)
        Write x -> liftIO $ writeFile x (unlines buf) >> pure ((),(buf,ln))

-- | Input line adds the given string to the buffer at the current line
--inputLines :: [String] -> (Buffer, Line) -> (Buffer, Line)
inputLines :: Applicative m => [String] -> Ed m ()
inputLines strs = Ed $ \(buf,ln) -> pure ((),(take ln buf ++ strs ++ drop ln buf, ln+(length strs)))

-- | Delete lines from the buffer in the given range. If the right bound of the
-- range is 'Nothing', delete just the specified line, that is, assume (x,x)
--deleteLines :: (Line, Maybe Line) -> Buffer -> Buffer
deleteLines :: Applicative m => (Line, Maybe Line) -> Ed m ()
deleteLines (x,y) = Ed $ \(buffer,line) ->
    pure ((),(deleteLines' (x, fromMaybe x y) buffer, line))
    where deleteLines' :: (Line, Line) -> Buffer -> Buffer
          deleteLines' (a,b) buffer = take (a-1) buffer ++ drop b buffer

-- | Get a range of lines from a buffer and return the partial buffer (the
-- selected lines) + the line number of the last line of the returned partial
-- buffer
--getLines :: (Line, Maybe Line) -> Buffer -> (Buffer, Line)
getLines :: Applicative m => (Line, Maybe Line) -> Ed m ()
getLines (x, y) = Ed $ \(buf,line) -> pure ((),getLines' (x, fromMaybe x y) buf)
    where
    getLines' :: (Line, Line) -> Buffer -> (Buffer, Line)
    getLines' (a,b) buf = (take (b-a+1) $ drop (a-1) buf, b)

-- The main function, `ed`, works on a buffer,
-- on a current line, and on a current mode
ed :: (Mode,(Buffer, Line)) -> InputT IO ()
ed (mode, (buffer, line)) = do
    userInput <- (fromMaybe "") <$> getInputLine ""
    case mode of
      CommandMode -> case parseCommand userInput of

          Nothing -> ed (mode,(buffer, line))

          Just (Left Quit) -> pure ()

          Just (Left ioCommand) -> runEd (executeIOCommand ioCommand) (buffer, line) >>= (\(_,x) -> ed (CommandMode,x))
          
          Just (Right command) -> ed $ runIdentity $ runEd (executeCommand command) (buffer, line)

      InputMode -> if userInput == "."            -- Change to command mode upon "." on a single line
          then ed (CommandMode, (buffer,line))
          else ed $ runIdentity $ runEd ((const InputMode) <$> (inputLines [userInput])) (buffer, line)

-- | Given a default value and a maybe value of the same type, return the value
-- if it's Just, return the default value if it's Nothing

fromMaybe :: a -> Maybe a -> a
fromMaybe k Nothing = k
fromMaybe k (Just l) = l

