module Data.Functor.Fix where

-- (1.5 балла) Реализуйте функцию `ffix`. Можете не пытаться её осознать,
-- просто следуйте за типами.
--
-- Подсказка: нужен ровно один `fmap` и рекурсия.

ffix ::
  Functor f =>
  -- | A data structure where every cell contains a function to compute the
  -- result for the cell based on the whole resulting data structure.
  f (f a -> a) ->
  f a
-- ^ @ffix@ is a function for computing a data structure @f@ of results of type
-- @a@ which might depend on the resulting data structure itself.
ffix x = go where go = fmap ($ go) x
