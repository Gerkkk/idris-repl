module Shell.Shell

import Core

public export
record ConsoleIO where
    constructor MkConsoleIO
    prompt  : String
    goodbye : String

export partial
runRepl : (Parser state, Formatter) => ConsoleIO -> state -> IO ()
runRepl console st = do
    putStr (prompt console)
    input <- getLine
    case parseInput input of
        Left e => do
            putStrLn ("Parse error: " ++ e)
            runRepl console st
        Right (MkCommand Quit) =>
            putStrLn (goodbye console)
        Right (MkCommand (Custom f)) =>
            case f st of
                Left e => do
                    putStrLn ("Error: " ++ e)
                    runRepl console st
                Right (val, newSt) => do
                    putStrLn (formatOutput (show val))
                    runRepl console newSt
