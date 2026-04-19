module Core

%default total

public export
data Command : (state : Type) -> (result : Type) -> Type where
    Quit : Command state ()
    Custom : Show result => (state -> Either String (result, state)) -> Command state result

public export
data SomeCommand : (state : Type) -> Type where
    MkCommand : Show a => Command state a -> SomeCommand state

public export
interface CommandParser state where
    parseCommand : String -> Either String (SomeCommand state)

public export
interface CommandExecutor state where
    executeCommand : Command state a -> state -> Either String (a, state)

public export
interface ResultFormatter state where
    formatResult : Command state a -> a -> String

public export
interface ReplState state where
    initialState : state

public export
interface ReplIO io where
    readInput : io -> IO (Either String String)
    writeOutput : io -> String -> IO (Either String ())
    getPrompt : io -> String
    getGoodbyeMessage : io -> String

isQuitString : String -> Bool
isQuitString "QUIT" = True
isQuitString _ = False

export partial
runRepl : (CommandParser state,
           CommandExecutor state,
           ResultFormatter state,
           ReplState state,
           ReplIO io)
        => io
        -> state
        -> IO ()
runRepl io st = do
    let prompt = getPrompt io
    _ <- writeOutput io prompt
    
    inputEither <- readInput io
    
    case inputEither of
        Left e => 
            if isQuitString e
                then do
                    let goodbye = getGoodbyeMessage io
                    _ <- writeOutput io goodbye
                    pure ()
                else do
                    _ <- writeOutput io ("Error: " ++ e)
                    runRepl io st
                    
        Right input =>
            case parseCommand input of
                Left e => do
                    _ <- writeOutput io ("Parse error: " ++ e)
                    runRepl io st
                    
                Right (MkCommand cmd) =>
                    case executeCommand cmd st of
                        Left e => do
                            if isQuitString e
                                then do
                                    _ <- writeOutput io (getGoodbyeMessage io)
                                    pure ()
                                else do
                                    _ <- writeOutput io ("Error: " ++ e)
                                    runRepl io st
                            
                        Right (result, newState) => do
                            let output = formatResult cmd result
                            _ <- writeOutput io output
                            runRepl io newState
