module Main

import Core
import Shell.Shell
import Calculator.State
import Calculator.Parser
import Calculator.Formatter

-- В Core определены интерфейсы:
--   Parser — разбирает строку в команду (Custom f | Quit). Параметризован типом хранилища
--   Formatter — форматирует вывод результата
-- Shell.Shell.runRepl требует оба интерфейса
--
-- Можно переопределить только реализацию вывода, написав runRepl
--
-- Можно написать другую логику repl-а, не меняя логику вывода
-- Для этого надо определить свой State + initialState, Parser, параметризованный новым State, а также Formatter для вывода новых данных,
-- передать MkConsoleIO и initialDbState в уже готовый runRepl через Main


main : IO ()
main = do
    putStrLn "Welcome to the calculator REPL!"
    putStrLn "You can create variables: 'let a1 = 2 * 7', 'let b4 = r2 * 7'"
    putStrLn "You can use variables: 'a1', 'u - 7'"
    putStrLn "You can evaluate integer expressions: '2 * 7', 'r2 * 7'"
    putStrLn "You can print all existing variables: ':show'"
    putStrLn "You can quit: ':quit'"
    let console = MkConsoleIO ">>> " "Goodbye!"
    runRepl console initialState
