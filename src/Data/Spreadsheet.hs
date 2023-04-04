{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Data.Spreadsheet where

import Control.Monad.Except (MonadError, throwError)
import Control.Monad.Free (Free (Pure), cutoff, iter)
import Data.Functor.Fix (ffix)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.Maybe (fromMaybe)

-- | A spreadsheet cell can be indexed by pair of @Int@s.
type SpreadsheetIndex = (Int, Int)

-- | A spreadsheet is a 2-dimensional array of possibly empty cells.
newtype Spreadsheet a = Spreadsheet
  {getSpreadsheet :: IntMap (IntMap a)}
  deriving (Functor)

-- | Returns cell in a spreadsheet by index
(!?) :: Spreadsheet a -> SpreadsheetIndex -> Maybe a
(!?) (Spreadsheet s) (row, col) = IM.lookup row s >>= IM.lookup col

insertCell :: SpreadsheetIndex -> a -> Spreadsheet a -> Spreadsheet a
insertCell (row, col) x (Spreadsheet s) =
  Spreadsheet $
    IM.alter
      ( \case
          Nothing -> Just $ IM.singleton col x
          Just rowMap -> Just $ IM.insert col x rowMap
      )
      row
      s

deleteCell :: SpreadsheetIndex -> Spreadsheet a -> Spreadsheet a
deleteCell (row, col) (Spreadsheet s) =
  Spreadsheet $
    IM.alter
      ( \case
          Nothing -> Nothing
          Just rowMap ->
            alterDecision $
              IM.delete
                col
                rowMap
      )
      row
      s
  where
    alterDecision rowMap' =
      if IM.null rowMap'
        then Nothing
        else Just rowMap'

getBounds :: Spreadsheet a -> SpreadsheetIndex
getBounds (Spreadsheet s) =
  ( maybe 0 fst $ IM.lookupMax s,
    IM.foldr (\rowMap bc -> maybe bc (max bc . fst) $ IM.lookupMax rowMap) 0 s
  )

-- | A @Formula f a@ in a spreadsheet cell can:
--
--    * reference results of other cells by index;
--
--    * use constants of type @a@;
--
--    * perform operations listed in grammar @f@.
type Formula f a = Free f (Either SpreadsheetIndex a)

-- | A type of errors which can happen during cell evaluation.
data EvalError
  = -- | Cell refers to an empty cell.
    BlankReferee
  | -- | There is a dependency cycle between cells.
    RecursionLimitExceeded

instance Show EvalError where
  show RecursionLimitExceeded = "#REF!"
  show BlankReferee = "#N/A!"

-- | A function to compute values in a spreadsheet.
calc ::
  (Traversable f, MonadError EvalError m) =>
  -- | How to perform operations listed in grammar @f@.
  (f (m a) -> m a) ->
  -- | Recursion limit.
  Integer ->
  -- | Spreadsheet of formulae.
  Spreadsheet (Formula f a) ->
  Spreadsheet (m a)
calc alg limit = fmap (evalExpression alg limit) . ffix . fmap (flip prepareExpression)
  where
    evalExpression ::
      (Traversable f, MonadError EvalError m) =>
      (f (m a) -> m a) ->
      Integer ->
      Free f (m a) ->
      m a
    evalExpression alg limit =
      iter alg
        . fmap (fromMaybe $ throwError RecursionLimitExceeded)
        . cutoff limit

    prepareExpression ::
      (Functor f, MonadError EvalError m) =>
      Spreadsheet (Free f (m a)) ->
      -- \^ Imagine that a spreadsheet of expressions @s@ is ready...
      Formula f a ->
      -- \^ And you are given a formula @f@ which may refer to cells in @s@.
      Free f (m a)
    -- \^ How do you substitute expressions in @f@
    -- to create another expression?
    prepareExpression s = (>>= either (fromMaybe (Pure $ throwError BlankReferee) . (s !?)) (Pure . return))
