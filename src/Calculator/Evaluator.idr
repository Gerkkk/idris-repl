module Calculator.Evaluator

import Calculator.State
import Data.List

%default total

public export
data Expr : Type -> Type where
    Lit  : Int -> Expr Int
    Add  : Expr Int -> Expr Int -> Expr Int
    Sub  : Expr Int -> Expr Int -> Expr Int
    Mul  : Expr Int -> Expr Int -> Expr Int
    Div  : Expr Int -> Expr Int -> Expr Int
    Mod  : Expr Int -> Expr Int -> Expr Int
    Pow  : Expr Int -> Expr Int -> Expr Int
    Var  : String -> Expr Int

-- Конвертирует Int в Nat для передачи в powInt и возвращает Nothing для отрицательных
intToNat : Int -> Maybe Nat
intToNat n =
    if n < 0
        then Nothing
        else Just (integerToNat (cast n))

-- Целочисленное возведение в степень через рекурсию по Nat.
powInt : Int -> Nat -> Int
powInt _ 0 = 1
powInt base (S exp) = base * powInt base exp

export
eval : Expr Int -> CalcState -> Either String Int
eval (Lit x) _ = Right x
eval (Add l r) st = do
    lv <- eval l st
    rv <- eval r st
    Right (lv + rv)
eval (Sub l r) st = do
    lv <- eval l st
    rv <- eval r st
    Right (lv - rv)
eval (Mul l r) st = do
    lv <- eval l st
    rv <- eval r st
    Right (lv * rv)
eval (Div l r) st = do
    lv <- eval l st
    rv <- eval r st
    if rv == 0
        then Left "Division by zero"
        else Right (div lv rv) 
eval (Mod l r) st = do
    lv <- eval l st
    rv <- eval r st
    if rv == 0
        then Left "By zero module"
        else Right (mod lv rv) 
eval (Pow l r) st = do
    lv <- eval l st
    rv <- eval r st
    case intToNat rv of
        Nothing => Left "Negative exponent not supported"
        Just n  => Right (powInt lv n)
eval (Var name) st = 
    case lookup name (vars st) of
        Nothing => Left ("Variable not found: " ++ name)
        Just v  => Right v