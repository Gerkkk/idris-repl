module Calculator.State

import Core

%default total

public export
record CalcState where
    constructor MkCalcState
    vars : List (String, Int)

export
ReplState CalcState where
    initialState = MkCalcState []
