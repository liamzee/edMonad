module Main where

import EdMonad ( ed, Mode(CommandMode) )
import System.Console.Haskeline ( defaultSettings, runInputT )

-- As described in the assignment,
-- ed starts in command mode
main :: IO ()
main = runInputT defaultSettings $ ed (CommandMode, ([],0))