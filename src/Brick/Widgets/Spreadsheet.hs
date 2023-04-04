{-# LANGUAGE OverloadedLabels #-}

module Brick.Widgets.Spreadsheet where

import Brick (get)
import Brick.Widgets.Edit (editContentsL)
import Brick.Widgets.List (listElementsL, listInsert, listRemove, listSelectedL)
import Brick.Widgets.TabularList.Grid (ColWidth (ColW), GridTabularList, Index (Ix))
import Control.Monad.Free (Free)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq, deleteAt, fromList, (|>))
import Data.Spreadsheet (Formula, Spreadsheet (Spreadsheet), SpreadsheetIndex, (!?))
import Lens.Micro ((%~), (&), (^.))
import Lens.Micro.Mtl ((.=))
import Text.Parsec.Formula (Value, showFormula)

type Cell f = Formula f Value

type SpreadsheetList n a = GridTabularList n (IntMap a)

defaultColWidth :: ColWidth
defaultColWidth = ColW 10

currentCol :: SpreadsheetList n a -> Int
currentCol sl = let Ix x = (sl ^. #currentColumn) in x

currentRow :: SpreadsheetList n a -> Int
currentRow sl = fromMaybe 0 $ sl ^. #list . listSelectedL

currentIndex :: SpreadsheetList n a -> SpreadsheetIndex
currentIndex sl = (currentRow sl, currentCol sl)

appendRow :: SpreadsheetList n a -> SpreadsheetList n a
appendRow sl = sl & #list %~ listInsert (currentRow sl + 1) IM.empty

appendCol :: SpreadsheetList n a -> SpreadsheetList n a
appendCol sl = sl & #widths %~ (|> defaultColWidth)

popRow :: SpreadsheetList n a -> SpreadsheetList n a
popRow sl = sl & #list %~ listRemove (currentRow sl)

popCol :: SpreadsheetList n a -> SpreadsheetList n a
popCol sl = sl & #widths %~ deleteAt (currentCol sl)

makeRows :: SpreadsheetIndex -> Spreadsheet a -> Seq (IntMap a)
makeRows (xn, _) (Spreadsheet s) =
  fromList $
    map
      (fromMaybe IM.empty . flip IM.lookup s)
      [0 .. xn]

emptySpreadsheet :: Spreadsheet a
emptySpreadsheet = Spreadsheet $ IM.fromList []