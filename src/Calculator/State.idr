module Calculator.State

%default total

public export
record CalcState where
    constructor MkCalcState
    vars : List (String, Int)

export
initialState : CalcState
initialState = MkCalcState []
