module Part2.Tasks where

import Util(notImplementedYet)

data BinaryOp = Plus | Minus | Times deriving (Show, Eq)

data Term = IntConstant { intValue :: Int }          -- числовая константа
          | Variable    { varName :: String }        -- переменная
          | BinaryTerm  { op :: BinaryOp, lhv :: Term, rhv :: Term } -- бинарная операция
             deriving(Show,Eq)

-- Для бинарных операций необходима не только реализация, но и адекватные
-- ассоциативность и приоритет
(|+|) :: Term -> Term -> Term
(|+|) a b = BinaryTerm Plus a b
(|-|) :: Term -> Term -> Term
(|-|) a b = BinaryTerm Minus a b
(|*|) :: Term -> Term -> Term
(|*|) a b = BinaryTerm Times a b

infixl 6 |+|
infixl 6 |-|
infixl 7 |*|

-- Заменить переменную `varName` на `replacement`
-- во всём выражении `expression`
replaceVar :: String -> Term -> Term -> Term
replaceVar varName replacement = replaceVarInner
  where
    replaceVarInner expression@(IntConstant _)         = expression
    replaceVarInner expression@(Variable var)          = if var == varName then replacement else expression
    replaceVarInner expression@(BinaryTerm op lvh rvh) = BinaryTerm op (replaceVarInner lvh) (replaceVarInner rvh)

-- Посчитать значение выражения `Term`
-- если оно состоит только из констант
evaluate :: Term -> Term
evaluate e@(IntConstant _)   = e
evaluate e@(Variable _)      = e
evaluate (BinaryTerm op a b) = case (aEval, bEval) of
  (IntConstant aInt, IntConstant bInt) -> IntConstant $ (binaryOpToIntOp op) aInt bInt
  (_, _) -> BinaryTerm op aEval bEval
  where
    aEval = evaluate a
    bEval = evaluate b

    binaryOpToIntOp Plus  = (+)
    binaryOpToIntOp Minus = (-)
    binaryOpToIntOp Times = (*)
