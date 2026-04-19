module Calculator.Parser

import Core
import Calculator.State
import Calculator.Evaluator
import Data.String

%default total

isVarName : String -> Bool
isVarName s = 
    case unpack s of
        [] => False
        (c :: cs) => isAlpha c && all isAlphaNum cs
  where
    isAlpha : Char -> Bool
    isAlpha c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
    
    isAlphaNum : Char -> Bool
    isAlphaNum c = isAlpha c || (c >= '0' && c <= '9')

export partial
parseExpr : String -> Either String (Expr Int)
parseExpr s = 
    let parts = words s in
    case parts of
        [token] => 
            if isVarName token
                then Right (Var token)
                else case parseInteger token of
                    Just n  => Right (Lit (cast n))
                    Nothing => Left ("Not a number or variable: " ++ token)
        [num] => case parseInteger num of
            Just n  => Right (Lit (cast n))
            Nothing => Left ("Not a number: " ++ num)
        [left, "+", right] => do
            l <- parseExpr left
            r <- parseExpr right
            Right (Add l r)
        [left, "-", right] => do
            l <- parseExpr left
            r <- parseExpr right
            Right (Sub l r)
        [left, "*", right] => do
            l <- parseExpr left
            r <- parseExpr right
            Right (Mul l r)
        [left, "/", right] => do
            l <- parseExpr left
            r <- parseExpr right
            Right (Div l r)
        [left, "%", right] => do
            l <- parseExpr left
            r <- parseExpr right
            Right (Mod l r)
        [left, "^", right] => do
            l <- parseExpr left
            r <- parseExpr right
            Right (Pow l r)
        _ =>  Left ("Invalid expression: " ++ s)

showState : CalcState -> Either String (String, CalcState)
showState st = Right (show (vars st), st)

export partial
CommandParser CalcState where
    parseCommand input = 
        let wordsList = words input in
        case wordsList of
            [":quit"] => Right (MkCommand Quit)
            [":show"] => Right (MkCommand (Custom showState))
            ("let" :: name :: "=" :: rest) => 
                let exprStr = unwords rest
                in case parseExpr exprStr of
                    Left err => Left err
                    Right expr => 
                        let cmd = Custom (\st => 
                                do val <- eval expr st
                                   let newVars = (name, val) :: vars st
                                   Right ((), MkCalcState newVars))
                        in Right (MkCommand cmd)
            _ => 
                case parseExpr input of
                    Left err => Left err
                    Right expr => 
                        let cmd = Custom (\st => 
                                do val <- eval expr st
                                   Right (val, st))
                        in Right (MkCommand cmd)