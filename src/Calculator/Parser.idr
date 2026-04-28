module Calculator.Parser

import Core
import Calculator.State
import Calculator.Evaluator
import Data.String

%default total

-- Переменная должна начинаться с буквы и состоять только из букв и цифр.
isVarName : String -> Bool
isVarName s =
    case unpack s of
        []       => False
        (c :: cs) => isAlpha c && all isAlphaNum cs
  where
    isAlpha : Char -> Bool
    isAlpha c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')

    isAlphaNum : Char -> Bool
    isAlphaNum c = isAlpha c || (c >= '0' && c <= '9')

export partial
parseExpr : String -> Either String (Expr Int)
parseExpr s =
    case words s of
        [token] =>
            if isVarName token
                then Right (Var token)
                else case parseInteger token of
                    Just n  => Right (Lit (cast n))
                    Nothing => Left ("Not a number or variable: " ++ token)
        [left, "+", right] => Add <$> parseExpr left <*> parseExpr right
        [left, "-", right] => Sub <$> parseExpr left <*> parseExpr right
        [left, "*", right] => Mul <$> parseExpr left <*> parseExpr right
        [left, "/", right] => Div <$> parseExpr left <*> parseExpr right
        [left, "%", right] => Mod <$> parseExpr left <*> parseExpr right
        [left, "^", right] => Pow <$> parseExpr left <*> parseExpr right
        _                  => Left ("Invalid expression: " ++ s)

partial
parseCommand : String -> Either String (SomeCommand CalcState)
parseCommand input =
    case words input of
        [":quit"] => Right (MkCommand Quit)
        [":show"] => Right (MkCommand (Custom (\st => Right (show (vars st), st))))
        ("let" :: name :: "=" :: rest) =>
            case parseExpr (unwords rest) of
                Left err   => Left err
                Right expr => Right (MkCommand (Custom (\st =>
                    do val <- eval expr st
                       Right ((), MkCalcState ((name, val) :: vars st)))))
        _ =>
            case parseExpr input of
                Left err   => Left err
                Right expr => Right (MkCommand (Custom (\st =>
                    do val <- eval expr st
                       Right (val, st))))

export partial
Parser CalcState where
    parseInput = parseCommand
