module Calculator.Formatter

import Core
import Calculator.State

%default total

export
ResultFormatter CalcState where
    formatResult Quit _ = ""
    formatResult (Custom _) result = 
        let str = show result in
        if str == "()" then "ok" else "= " ++ str