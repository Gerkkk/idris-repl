module Calculator.Formatter

import Core

%default total

export
Formatter where
    formatOutput "()" = "ok"
    formatOutput str  = "= " ++ str
