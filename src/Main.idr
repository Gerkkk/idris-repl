module Main

import Core
import Shell
import Calculator.State
import Calculator.Parser
import Calculator.Evaluator
import Calculator.Executor
import Calculator.Formatter


main : IO ()
main = 
    let io = MkConsoleIO
        st = initialState {state=CalcState}
    in runRepl io st
