{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE NoFieldSelectors #-}

module Main where

import Brick.AttrMap
  ( AttrName,
    attrMap,
    attrName,
  )
import Brick.Focus
  ( FocusRing,
    focusGetCurrent,
    focusNext,
    focusRing,
    withFocusRing,
  )
import Brick.Main
  ( App
      ( App,
        appAttrMap,
        appChooseCursor,
        appDraw,
        appHandleEvent,
        appStartEvent
      ),
    defaultMain,
    halt,
    showFirstCursor,
  )
import Brick.Types
  ( BrickEvent (VtyEvent),
    EventM,
    Widget,
    get,
    modify,
    zoom,
  )
import Brick.Util
  ( bg,
    fg,
    on,
  )
import Brick.Widgets.Border
  ( border,
    hBorder,
  )
import Brick.Widgets.Center (hCenter, vCenter)
import Brick.Widgets.Core
  ( Padding (Max, Pad),
    emptyWidget,
    fill,
    forceAttr,
    joinBorders,
    padLeft,
    padLeftRight,
    padRight,
    str,
    vLimit,
    withAttr,
    (<=>),
  )
import Brick.Widgets.Edit
  ( Editor,
    editor,
    getEditContents,
    handleEditorEvent,
    renderEditor,
  )
import Brick.Widgets.List
  ( listMoveDown,
    listMoveTo,
    listMoveUp,
  )
import Brick.Widgets.Spreadsheet
  ( Cell,
    SpreadsheetList,
    appendCol,
    appendRow,
    currentIndex,
    defaultColWidth,
    emptySpreadsheet,
    makeRows,
    popCol,
    popRow,
  )
import Brick.Widgets.TabularList.Grid
  ( ColHdrHeight (ColHdrH),
    ColHdrRowHdr (ColHdrRowHdr),
    GridColCtxt (GColC),
    GridColHdr (..),
    GridCtxt (GrdCtxt),
    GridRenderers (..),
    GridRowCtxt (GRowC),
    Index (Ix),
    ListFocused (LstFcs),
    ListItemHeight (LstItmH),
    RowHdr (..),
    RowHdrCtxt (RowHdrCtxt),
    RowHdrWidth (RowHdrW),
    Selected (Sel),
    WidthDeficit (WdthD),
    gridMoveLeft,
    gridMoveRight,
    gridMoveTo,
    gridTabularList,
    renderGridTabularList,
  )
import Control.Monad (void, when)
import Data.IntMap (IntMap)
import Data.IntMap qualified as IM
import Data.Sequence qualified as S
import Data.Spreadsheet
  ( Spreadsheet,
    SpreadsheetIndex,
    calc,
    deleteCell,
    getBounds,
    insertCell,
    (!?),
  )
import GHC.Generics (Generic)
import Graphics.Vty
  ( Event (..),
    Key (..),
    black,
    blue,
    defAttr,
    red,
    white,
  )
import Interpreters
  ( AllDialect,
    AllResult,
    runAll,
    showAllResult,
  )
import Lens.Micro
  ( (^.),
    _1,
    _2,
  )
import Lens.Micro.Mtl
  ( use,
    (%=),
    (.=),
    (<%=),
    (<.=),
  )
import Text.Parsec.Formula
  ( Value,
    intToBase26,
    parseFormula,
    showFormula,
  )

data Name = NSpreadsheet | NEditor deriving (Eq, Ord, Show)

data AppState = AppState
  { spreadsheet :: Spreadsheet (Cell AllDialect),
    spreadsheetList :: SpreadsheetList Name (AllResult Value),
    spreadsheetRenderers :: SpreadsheetRenderers (AllResult Value),
    bounds :: SpreadsheetIndex,
    focusRing :: FocusRing Name,
    formulaEditor :: Editor String Name,
    editorError :: Bool,
    showHelp :: Bool
  }
  deriving (Generic)

type SpreadsheetRenderers a = GridRenderers Name (IntMap a)

updateEditor :: EventM Name AppState ()
updateEditor = do
  s <- get
  let ss = s ^. #spreadsheet
  let idx = currentIndex (s ^. #spreadsheetList)
  #formulaEditor .= newFormulaEditor (ss !? idx)

handleSpreadsheetEvent :: BrickEvent Name () -> EventM Name AppState ()
handleSpreadsheetEvent e = do
  s <- get
  let (row, col) = currentIndex (s ^. #spreadsheetList)
  case e of
    VtyEvent (EvKey KDown []) -> do
      when (s ^. #bounds . _1 <= row) (#spreadsheetList %= appendRow)
      #spreadsheetList . #list %= listMoveDown
      updateEditor
    VtyEvent (EvKey KUp []) -> do
      if s ^. #bounds . _1 < row
        then #spreadsheetList %= popRow
        else #spreadsheetList . #list %= listMoveUp
      updateEditor
    VtyEvent (EvKey KRight []) -> do
      when (s ^. #bounds . _2 <= col) (#spreadsheetList %= appendCol)
      #spreadsheetList %= gridMoveRight
      updateEditor
    VtyEvent (EvKey KLeft []) -> do
      #spreadsheetList %= gridMoveLeft
      when (s ^. #bounds . _2 < col) (#spreadsheetList %= popCol)
      updateEditor
    _ -> return ()

handleEvent :: BrickEvent Name () -> EventM Name AppState ()
handleEvent e = do
  case e of
    VtyEvent (EvKey (KChar 'h') []) -> #showHelp %= not
    VtyEvent (EvKey KEsc []) -> halt
    VtyEvent (EvKey (KChar 'q') []) -> halt
    VtyEvent (EvKey (KChar 'r') []) -> zoom (#spreadsheetRenderers . #rowHdr) $ modify $ \case
      Nothing -> Just rowHdr
      Just _ -> Nothing
    VtyEvent (EvKey (KChar 'c') []) -> zoom (#spreadsheetRenderers . #colHdr) $ modify $ \case
      Nothing -> Just colHdr
      Just _ -> Nothing
    VtyEvent (EvKey KEnter []) -> do
      r <- use #focusRing
      case focusGetCurrent r of
        Just NEditor -> do
          input <- head . getEditContents <$> use #formulaEditor
          case input of
            "" -> do
              idx <- currentIndex <$> use #spreadsheetList
              s <- #spreadsheet <%= deleteCell idx
              let (row, col) = idx
              (br, bc) <- #bounds <.= getBounds s
              #spreadsheetList .= calcList (max br row, max bc col) s
              #spreadsheetList %= gridMoveTo (Ix col)
              #spreadsheetList . #list %= listMoveTo row
              #focusRing %= focusNext
            _ -> do
              let formula = parseFormula input
              case formula of
                Left _ -> #editorError .= True
                Right f -> do
                  idx <- currentIndex <$> use #spreadsheetList
                  s <- #spreadsheet <%= insertCell idx f
                  let (row, col) = idx
                  b <- #bounds <%= \(br, bc) -> (max br row, max bc col)
                  #spreadsheetList .= calcList b s
                  #spreadsheetList %= gridMoveTo (Ix col)
                  #spreadsheetList . #list %= listMoveTo row
                  updateEditor
                  #focusRing %= focusNext
        _ -> #focusRing %= focusNext
    ev -> do
      r <- use #focusRing
      case focusGetCurrent r of
        Just NSpreadsheet -> handleSpreadsheetEvent ev
        Just NEditor -> do
          zoom #formulaEditor $ handleEditorEvent ev
          #editorError .= False
        Nothing -> return ()

newFormulaEditor :: Maybe (Cell AllDialect) -> Editor String Name
newFormulaEditor f = editor NEditor (Just 1) $ maybe "" showFormula f

renderFormulaEditor :: Bool -> Editor String Name -> Widget Name
renderFormulaEditor = renderEditor (str . unlines)

helpText :: Widget Name
helpText =
  hBorder
    <=> hCenter (str "Press h to toggle help")
    <=> hCenter (str "Press Esc or q to exit")
    <=> hCenter (str "Press c to toggle column headers")
    <=> hCenter (str "Press r to toggle row headers")
    <=> hCenter (str "Press Enter to edit cell")
    <=> hCenter (str "Navigate with arrow keys")

drawUi :: AppState -> [Widget Name]
drawUi s =
  let renderedList =
        renderGridTabularList
          (s ^. #spreadsheetRenderers)
          (LstFcs (focusGetCurrent (s ^. #focusRing) == Just NSpreadsheet))
          (s ^. #spreadsheetList)
      errorAttrFn = if s ^. #editorError then forceAttr editorErrorAttr else id
      renderedEditor =
        errorAttrFn $
          withFocusRing
            (s ^. #focusRing)
            renderFormulaEditor
            (s ^. #formulaEditor)
      help = if s ^. #showHelp then helpText else emptyWidget
   in [ vCenter . padLeftRight 2 . joinBorders . border $
          ( renderedList
              <=> help
              <=> hBorder
              <=> renderedEditor
          )
      ]

columnHdrAttr :: AttrName
columnHdrAttr = attrName "columnHeader"

rowHdrAttr :: AttrName
rowHdrAttr = attrName "rowHeader"

editorErrorAttr :: AttrName
editorErrorAttr = attrName "editorError"

colSelectedAttr :: AttrName
colSelectedAttr = attrName "selectedColumn"

rowHdr :: RowHdr Name (IntMap a)
rowHdr =
  RowHdr
    { draw = \_ (WdthD wd) (RowHdrCtxt (Sel s)) r ->
        let attrFn =
              if s
                then id
                else withAttr rowHdrAttr
         in attrFn $ padRight (Pad $ if wd > 0 then 0 else 1) $ padLeft Max (str $ show r),
      width = \_ rs -> RowHdrW $ (+ 2) $ maximum $ map (length . show) rs,
      toRH = \_ (Ix i) -> i
    }

colHdr :: GridColHdr Name
colHdr =
  GridColHdr
    { draw = \_ (WdthD wd) (GColC (Ix ci) _) ->
        ( withAttr columnHdrAttr
            . padRight (Pad $ if wd > 0 then 0 else 1)
            . padRight Max
            . str
        )
          (intToBase26 ci)
          <=> hBorder,
      height = ColHdrH 2
    }

renderers :: SpreadsheetRenderers (AllResult Value)
renderers =
  GridRenderers
    { cell = \_ (WdthD wd) (GrdCtxt (GRowC _ (Sel rs)) (GColC (Ix ci) (Sel cs))) s ->
        let attrFn =
              if rs && cs
                then withAttr colSelectedAttr
                else id
            renderCell s' = padRight (Pad $ if wd > 0 then 0 else 1) (padRight Max $ str s')
         in attrFn $ maybe (fill ' ') (renderCell . showAllResult) (s IM.!? ci),
      rowHdr = Just rowHdr,
      colHdr = Just colHdr,
      colHdrRowHdr = Just $ ColHdrRowHdr $ \_ _ -> vLimit 1 (fill ' ') <=> hBorder
    }

calcList ::
  SpreadsheetIndex ->
  Spreadsheet (Cell AllDialect) ->
  SpreadsheetList Name (AllResult Value)
calcList b s =
  gridTabularList
    NSpreadsheet
    (makeRows b $ calc runAll 500 s)
    (LstItmH 1)
    (S.fromList $ replicate (snd b + 1) defaultColWidth)

main :: IO ()
main = do
  let spreadsheet = emptySpreadsheet
      bounds = (0, 0)
      appState =
        AppState
          { spreadsheet = spreadsheet,
            spreadsheetList = calcList bounds spreadsheet,
            spreadsheetRenderers = renderers,
            bounds = bounds,
            focusRing = focusRing [NSpreadsheet, NEditor],
            formulaEditor = newFormulaEditor Nothing,
            editorError = False,
            showHelp = True
          }
      app =
        App
          { appDraw = drawUi,
            appChooseCursor = showFirstCursor,
            appHandleEvent = handleEvent,
            appStartEvent = return (),
            appAttrMap =
              const $
                attrMap
                  defAttr
                  [ (colSelectedAttr, black `on` white),
                    (columnHdrAttr, fg blue),
                    (rowHdrAttr, fg red),
                    (editorErrorAttr, bg red)
                  ]
          }
  void $ defaultMain app appState
