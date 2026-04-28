module Core

%default total

public export
data Command : (state : Type) -> (result : Type) -> Type where
    Quit   : Command state ()
    Custom : Show result => (state -> Either String (result, state)) -> Command state result

public export
data SomeCommand : (state : Type) -> Type where
    MkCommand : Command state a -> SomeCommand state

public export
interface Parser (0 state : Type) where
    parseInput : String -> Either String (SomeCommand state)

public export
interface Formatter where
    formatOutput : String -> String
