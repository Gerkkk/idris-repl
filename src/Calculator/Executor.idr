module Calculator.Executor

import Core
import Calculator.State

%default total

export
CommandExecutor CalcState where
    executeCommand Quit _ = Left "QUIT"
    executeCommand (Custom f) st = f st