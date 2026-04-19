module Shell

import Core

%default total

public export
data ConsoleIO = MkConsoleIO

export
ReplIO ConsoleIO where
    readInput _ = do
        line <- getLine
        pure (Right line)
    
    writeOutput _ msg = do
        putStrLn msg
        pure (Right ())
    
    getPrompt _ = ">>> "
    getGoodbyeMessage _ = "Goodbye!"
