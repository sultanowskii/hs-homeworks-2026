module Part4.Tasks where

import Util(notImplementedYet)

-- Перевёрнутый связный список -- хранит ссылку не на последующию, а на предыдущую ячейку
data ReverseList a = REmpty | (ReverseList a) :< a
infixl 5 :<

-- Функция-пример, делает из перевёрнутого списка обычный список
-- Использовать rlistToList в реализации классов запрещено =)
rlistToList :: ReverseList a -> [a]
rlistToList lst =
    reverse (reversed lst)
    where reversed REmpty = []
          reversed (init :< last) = last : reversed init

-- Реализуйте обратное преобразование
listToRlist :: [a] -> ReverseList a
listToRlist lst =
  to $ reverse lst
  where
    to []    = REmpty
    to (h:l) = (to l) :< h

-- Реализуйте все представленные ниже классы (см. тесты)
instance (Show a) =>  Show (ReverseList a)  where
    show lst =
      "[" ++ (showContents lst "") ++ "]"
      where
        showContents REmpty acc        = acc
        showContents (REmpty :< h) acc = show h ++ acc
        showContents (l :< h) acc      = showContents l ("," ++ show h ++ acc)

instance (Eq a) => Eq (ReverseList a) where
    (==) REmpty REmpty         = True
    (==) (l1 :< h1) (l2 :< h2) = h1 == h2 && l1 == l2
    (==) _ _                   = False

instance Semigroup (ReverseList a) where
  (<>) a b = merge b
    where
      merge REmpty = a
      merge (l :< h)  = (merge l) :< h

instance Monoid (ReverseList a) where
  mempty = REmpty

instance Functor ReverseList where
  fmap f REmpty   = REmpty
  fmap f (l :< h) = (fmap f l) :< (f h)

instance Applicative ReverseList where
  pure el = REmpty :< el

  (<*>) REmpty lst    = REmpty
  (<*>) (fs :< f) lst = (fs <*> lst) <> (fmap f lst)

instance Monad ReverseList where
  (>>=) REmpty f = REmpty
  (>>=) (l :< h) f = (l >>= f) <> (f h)