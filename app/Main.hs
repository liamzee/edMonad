module Main where

import EdMonad

-- As described in the assignment,
-- ed starts in command mode
main :: IO ()
main = ed ([], 0, CommandMode)