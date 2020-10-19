{-# LANGUAGE QuasiQuotes                      #-}

module Main where

import System.IO (stdin, hSetEcho, hSetBuffering, hReady, BufferMode (NoBuffering) )
import System.Console.ANSI
import Control.Monad (when, forM_)
import Data.List (find, sortBy, isInfixOf, intercalate)
import Data.Ord (Ordering(LT,EQ,GT))
import Text.RE.TDFA.String

import Lib

rows :: [RowData]
rows = [
    Row 1 "one" "smth-one" 101,
    Row 2 "two" "smth-two" 102,
    Row 3 "tree" "smth-tree" 103,

    Row 4 "four" "smth-four" 104,
    Row 5 "five" "smth-five" 105,
    Row 6 "six" "smth-six" 106,

    Row 7 "seven" "smth-seven" 107,
    Row 8 "eight" "smth-eight" 108,
    Row 9 "nine" "smth-nine" 109,

    Row 10 "ten" "profess-ten" 110,
    Row 11 "eleven" "profess-eleven" 111,
    Row 12 "twelve" "profess-twelve" 112,

    Row 10 "ten-one-first" "profess-ten" 110,
    Row 10 "ten-one-second" "profess-eleven" 111,
    Row 10 "ten-one-third" "profess-twelve" 112
  ]

data SortDirection = Asc | Desc

data PagingInfoData = PagingInfo { getRowsPerPage :: Int, getOffset :: Int }

data AppStateData = AppState {
  getRows :: [RowData],
  getPagingInfo :: PagingInfoData,
  getSorts :: [(String, SortDirection)],
  getFilters :: [(String, String)] }

stateWithNewRowsPerPage :: Int -> AppStateData -> AppStateData
stateWithNewRowsPerPage rowsPerPage (AppState appRows (PagingInfo _ rowOffset) sorts filters) =
  AppState appRows (PagingInfo rowsPerPage rowOffset) sorts filters

stateWithNewOffset :: Int -> AppStateData -> AppStateData
stateWithNewOffset offset (AppState appRows (PagingInfo rowsPerPage _) sorts filters) =
  AppState appRows (PagingInfo rowsPerPage offset) sorts filters

stateWithNewSort :: AppStateData -> String -> SortDirection -> AppStateData
stateWithNewSort appState@(AppState appRows (PagingInfo rowsPerPage rowOffset) sorts filters) columnName sortDirection =
  let
    newSorts = case (find (\(cn,_) -> cn == columnName) sorts) of
      Nothing -> sorts ++ [(columnName, sortDirection)]
      Just _ -> map (\p@(cn, _) -> if cn == columnName then (cn, sortDirection) else p) sorts
    currentRows = tmpApplySortsAndFilters newSorts filters rows
  in AppState currentRows (PagingInfo rowsPerPage 0) newSorts filters

stateWithNoSortForColumn :: AppStateData -> String -> AppStateData
stateWithNoSortForColumn appState@(AppState appRows (PagingInfo rowsPerPage rowOffset) sorts filters) columnName =
  let
    newSorts = filter (\(cn,_) -> cn /= columnName) sorts
    currentRows = tmpApplySortsAndFilters newSorts filters rows
  in AppState currentRows (PagingInfo rowsPerPage 0) newSorts filters

stateWithNewFilter :: AppStateData -> String -> String -> AppStateData
stateWithNewFilter appState@(AppState appRows (PagingInfo rowsPerPage rowOffset) sorts filters) columnName fltr =
  let
    newFilters = if fltr == ""
      then
        filter (\(cn, _) -> cn /= columnName) filters
      else
        case (find (\(cn,_) -> cn == columnName) filters) of
          Nothing -> filters ++ [(columnName, fltr)]
          Just _ -> map (\p@(cn, _) -> if cn == columnName then (cn, fltr) else p) filters
    currentRows = tmpApplySortsAndFilters sorts newFilters rows
  in AppState currentRows (PagingInfo rowsPerPage 0) sorts newFilters

tmpApplySortsAndFilters :: [(String, SortDirection)] -> [(String, String)] -> [RowData] -> [RowData]
tmpApplySortsAndFilters sorts filters rows =
  newRows
  where
  filterByColumn columnName fltr rs = case columnName of
    "a" -> filter (\row -> isInfixOf fltr (show . getA $ row)) rs
    "b" -> filter (\row -> isInfixOf fltr (getB row)) rs
    "c" -> filter (\row -> isInfixOf fltr (getC row)) rs
    "d" -> filter (\row -> isInfixOf fltr (show . getD $ row)) rs
--  sortByColumnAndDirection columnName sortDirection rs = case sortDirection of
--    Asc -> sortByColumnAsc columnName rs
--    Desc -> sortByColumnDesc columnName rs
--  sortByColumnAsc columnName rs = case columnName of
--    "a" -> sortBy (\r1 r2 -> compare (getA r1) (getA r2)) rs
--    "b" -> sortBy (\r1 r2 -> compare (getB r1) (getB r2)) rs
--    "c" -> sortBy (\r1 r2 -> compare (getC r1) (getC r2)) rs
--    "d" -> sortBy (\r1 r2 -> compare (getD r1) (getD r2)) rs
--  sortByColumnDesc columnName rs = case columnName of
--    "a" -> sortBy (\r1 r2 -> compare (getA r2) (getA r1)) rs
--    "b" -> sortBy (\r1 r2 -> compare (getB r2) (getB r1)) rs
--    "c" -> sortBy (\r1 r2 -> compare (getC r2) (getC r1)) rs
--    "d" -> sortBy (\r1 r2 -> compare (getD r2) (getD r1)) rs
  filteredRows = foldr (\(cn, fltr) rs -> filterByColumn cn fltr rs) rows filters
  -- TODO every subsequent sort cancels previous sort results
--  sortedRows = foldr (\(cn, sd) rs -> sortByColumnAndDirection cn sd rs) filteredRows sorts
  compareRows r1 r2 =
    let
      compareByColumn columnName sortDirection = case sortDirection of
        Asc -> compareByColumnAsc columnName
        Desc -> compareByColumnDesc columnName
      compareByColumnAsc columnName = case columnName of
        "a" -> compare (getA r1) (getA r2)
        "b" -> compare (getB r1) (getB r2)
        "c" -> compare (getC r1) (getC r2)
        "d" -> compare (getD r1) (getD r2)
      compareByColumnDesc columnName = case columnName of
        "a" -> compare (getA r2) (getA r1)
        "b" -> compare (getB r2) (getB r1)
        "c" -> compare (getC r2) (getC r1)
        "d" -> compare (getD r2) (getD r1)
      result = foldr (\(cn, sd) ord -> if ord /= EQ then ord else compareByColumn cn sd) EQ (reverse sorts)
    in result
  sortedRows = sortBy compareRows filteredRows
  newRows = sortedRows


main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  let
    pagingInfo = PagingInfo 3 0
    appState = AppState rows pagingInfo [("a", Asc), ("c", Desc)] [("a", "some"), ("b", "fltr")]
    --appState = AppState rows pagingInfo [("a", Asc), ("c", Desc)] []
  mainLoop appState

mainLoop :: AppStateData -> IO ()
mainLoop appState@(AppState appRows (PagingInfo rowsPerPage rowOffset) sorts filters) = do
  printTable appState Nothing
  key <- getKey
  when (key /= "\ESC") $ do
    case key of
      "\ESC[A" -> do -- up
        let
          rowsCount = length appRows
          newRowsPerPage = if rowsPerPage >= rowsCount then rowsCount else rowsPerPage + 1
        mainLoop (stateWithNewRowsPerPage newRowsPerPage appState)
      "\ESC[B" -> do -- down
        let
          newRowsPerPage = if rowsPerPage <= 1 then 1 else rowsPerPage - 1
        mainLoop (stateWithNewRowsPerPage newRowsPerPage appState)
      "\ESC[C" -> do -- right
        let
          rowsCount = length appRows
          offset = if rowOffset >= rowsCount - 1 then rowsCount - 1 else rowOffset + 1
        mainLoop (stateWithNewOffset offset appState)
      "\ESC[D" -> do -- left
        let offset = if rowOffset <= 0 then 0 else rowOffset - 1
        mainLoop (stateWithNewOffset offset appState)
      "\n" -> do -- enter
        editMode appState 0
      "q" -> return ()
      _ -> return ()

editMode :: AppStateData -> Int -> IO ()
editMode appState@(AppState appRows (PagingInfo rowsPerPage rowOffset) sorts filters) columnIndex = do
  printTable appState (Just $ EditModeInfo columnIndex)
  key <- getKey
  case key of
    "\ESC[A" -> do -- up
      let
        rowsCount = length rows
        newRowsPerPage = if rowsPerPage >= rowsCount then rowsCount else rowsPerPage + 1
      mainLoop (stateWithNewRowsPerPage newRowsPerPage appState)
    "\ESC[B" -> do -- down
      let
        newRowsPerPage = if rowsPerPage <= 1 then 1 else rowsPerPage - 1
      mainLoop (stateWithNewRowsPerPage newRowsPerPage appState)
    "\ESC[C" -> do -- right
      let
        newColumnIndex = if columnIndex >= 3 then columnIndex else columnIndex + 1
      editMode appState newColumnIndex
    "\ESC[D" -> do -- left
      let
        newColumnIndex = if columnIndex <= 0 then columnIndex else columnIndex - 1
      editMode appState newColumnIndex
    "\n" -> do -- enter
      mainLoop appState
    "q" -> return ()
    "s" -> do
      --return () -- TODO show sort direction dialog (asc, desc, no sorting)
      sortDirectionDialog (("a":"b":"c":"d":[]) !! columnIndex) appState
    "f" -> do
      --return () -- TODO show filter dialog
      let
        columnName = (("a":"b":"c":"d":[]) !! columnIndex)
        fs = (getFilters appState)
        fltr = case (find (\(cn,_) -> cn == columnName) filters) of
          Nothing -> ""
          Just (_,f) -> f
      filterDialog columnName appState fltr
    _ -> return ()

filterDialog :: String -> AppStateData -> String -> IO ()
filterDialog columnName appState tmpFilter = do
  let
    txt = "enter filter for column \"" ++ columnName ++ "\":"
    lentxt = length txt
    topStr    = "┌" ++ (replicate lentxt '─') ++ "┐"
    middleStr = "│" ++ (replicate lentxt ' ') ++ "│"
    bottomStr = "└" ++ (replicate lentxt '─') ++ "┘"
    yPos = 5
    xPos = 10
  saveCursor
  setCursorPosition yPos xPos
  putStr topStr
  setCursorPosition (yPos+1) xPos
  putStr middleStr
  setCursorPosition (yPos+2) xPos
  putStr middleStr
  setCursorPosition (yPos+1) (xPos+1)
  putStr txt
  setCursorPosition (yPos+2) (xPos+1)
  putStr tmpFilter
  setCursorPosition (yPos+3) xPos
  putStr bottomStr
  restoreCursor
  key <- getKey
  case key of
    "\n" -> mainLoop (stateWithNewFilter appState columnName tmpFilter)
    "\DEL" -> filterDialog columnName appState (if (length tmpFilter) == 0 then tmpFilter else init tmpFilter)
    c -> filterDialog columnName appState (tmpFilter ++ c)

sortDirectionDialog :: String -> AppStateData -> IO ()
sortDirectionDialog columnName appState = do
  let
    txt = "choose sort direction for column \"" ++ columnName ++ "\":"
    txtAsc = "a - ascending"
    txtDesc = "d - descending"
    txtNo = "n - no sort"
    lentxt = length txt
    topStr    = "┌" ++ (replicate lentxt '─') ++ "┐"
    middleStr = "│" ++ (replicate lentxt ' ') ++ "│"
    bottomStr = "└" ++ (replicate lentxt '─') ++ "┘"
    yPos = 5
    xPos = 10
  saveCursor
  setCursorPosition yPos xPos
  putStr topStr
  forM_ [1..5] $ \i -> do
    setCursorPosition (yPos+i) xPos
    putStr middleStr
  setCursorPosition (yPos+5) xPos
  putStr bottomStr
  setCursorPosition (yPos+1) (xPos+1)
  putStr txt
  setCursorPosition (yPos+2) (xPos+1)
  putStr txtAsc
  setCursorPosition (yPos+3) (xPos+1)
  putStr txtDesc
  setCursorPosition (yPos+4) (xPos+1)
  putStr txtNo
  restoreCursor
  key <- getKey
  case key of
    "a" -> mainLoop (stateWithNewSort appState columnName Asc)
    "d" -> mainLoop (stateWithNewSort appState columnName Desc)
    "n" -> mainLoop (stateWithNoSortForColumn appState columnName)
    _ -> sortDirectionDialog columnName appState

--dialogSortOrFilter :: IO ()
--dialogSortOrFilter = do
--  printSorts
--  printFilters
--  key <- getKey
--  case key of

data EditModeInfoData = EditModeInfo { columnIndex :: Int }

-- TODO two modes: regular and "sorts and filters editing" mode
printTable :: AppStateData -> Maybe EditModeInfoData -> IO ()
printTable appState@(AppState appRows (PagingInfo rowsPerPage rowOffset) sorts filters) maybeEditModeInfo = do
  let
    rowsToPrint = take rowsPerPage (drop rowOffset appRows)
    columnWidth = 14
    columnCount = 4
    topStr         = "┌" ++ (intercalate "┬" (replicate columnCount (replicate columnWidth '─'))) ++ "┐"
    rowBoxStr      = "│" ++ (intercalate "│" (replicate columnCount (replicate columnWidth ' '))) ++ "│"
    betweenRowsStr = "├" ++ (intercalate "┼" (replicate columnCount (replicate columnWidth '─'))) ++ "┤"
    bottomStr      = "└" ++ (intercalate "┴" (replicate columnCount (replicate columnWidth '─'))) ++ "┘"
    x0 = 0
    y0 = 0
    printRowBox rowIndex = do
      if rowIndex == 0
        then do
          setCursorPosition (y0+1) x0
          putStr rowBoxStr
        else do
          setCursorPosition (y0+rowIndex*2) x0
          putStr betweenRowsStr
          setCursorPosition (y0+rowIndex*2+1) x0
          putStr rowBoxStr
    firstRowIndex = if (length filters) > 0 then 2 else 1
    printRows = do
      forM_ (zip rowsToPrint [firstRowIndex..]) $ \(row, rowIndex) -> do
        let
          a = show . getA $ row
          b = getB row
          c = getC row
          d = show . getD $ row
        printRowValues (a:b:c:d:[]) rowIndex
    highlightCurrentColumn = do
      case maybeEditModeInfo of
        Nothing -> return ()
        Just(EditModeInfo columnIndex) -> do
          let
            yPos = y0
            xPosLeft = x0 + (columnWidth+1)*columnIndex
            xPosRight = xPosLeft + 1 + columnWidth
            leftUpperCorner = if columnIndex == 0 then "┏" else "┲"
            leftBottomCorner = if columnIndex == 0 then "┡" else "╄"
            rightUpperCorner = if columnIndex == 3 then "┓" else "┱"
            rightBottomCorner = if columnIndex == 3 then "┩" else "╃"
            topStr = leftUpperCorner ++ (replicate columnWidth '━') ++ rightUpperCorner
            bottomStr = leftBottomCorner ++ (replicate columnWidth '━') ++ rightBottomCorner
          setCursorPosition yPos xPosLeft
          putStr topStr
          setCursorPosition (yPos+2) xPosLeft
          putStr bottomStr
          setCursorPosition (yPos+1) xPosLeft
          putStr "┃"
          setCursorPosition (yPos+1) xPosRight
          putStr "┃"
    printRowValues (a : b : c : d : _) rowIndex = do
      let
        yPos = y0+1+rowIndex*2
        x1 = x0+1
        x2 = x0+1+columnWidth+1
        x3 = x0+1+(columnWidth+1)*2
        x4 = x0+1+(columnWidth+1)*3
      setCursorPosition yPos x1
      putStr a
      setCursorPosition yPos x2
      putStr b
      setCursorPosition yPos x3
      putStr c
      setCursorPosition yPos x4
      putStr d
    columnNames = ("a":"b":"c":"d":[])
    sortDirectionOf :: [(String, SortDirection)] -> String -> Maybe SortDirection
    sortDirectionOf [] columnName = Nothing
    sortDirectionOf ((cn, sd):sortz) columnName = if cn == columnName then Just(sd) else sortDirectionOf sortz columnName
    strN n str = concat (replicate n str)
    printHeader = do
      let
        sortDirectionsOptional = map (sortDirectionOf sorts) columnNames
        sortDirectionSymbol :: Maybe SortDirection -> String
        sortDirectionSymbol (Nothing) = ""
        sortDirectionSymbol (Just(Asc)) = "↑"
        sortDirectionSymbol (Just(Desc)) = "↓"
        appendSortDirection columnName = columnName ++ (strN (columnWidth - (length columnName) - 1) " ") ++ (sortDirectionSymbol (sortDirectionOf sorts columnName))
        columnNamesWithSorts = map appendSortDirection columnNames
      printRowValues columnNamesWithSorts 0
    filterOf [] columnName = Nothing
    filterOf ((cn, term):fs) columnName = if cn == columnName then Just(term) else filterOf fs columnName
    printFilters = do
      let
        filterTermsOptional = map (filterOf filters) columnNames
        optToStr :: Maybe String -> String
        optToStr (Nothing) = ""
        optToStr (Just(s)) = s
        filterTerms = map optToStr filterTermsOptional
      if (length filters) > 0
        then printRowValues filterTerms 1
        else return ()
    countRowsInBox = if (length filters) > 0 then (length rowsToPrint) + 1 else (length rowsToPrint)
  saveCursor
  clearScreen
  setCursorPosition y0 x0
  putStr topStr
  forM_ [0 .. countRowsInBox] printRowBox
  setCursorPosition (y0+(countRowsInBox + 1)*2) x0
  putStr bottomStr
  printHeader
  highlightCurrentColumn
  printFilters
  printRows
  restoreCursor
  --setCursorPosition (y0+(length rowsToPrint)*2+3) x0
  --forM_ rowsToPrint (putStrLn . show)


--loop :: IO ()
--loop = do
--  putStr ">"
--  line <- getLine
--  case line of
--    "q" -> do
--      putStrLn "Bye"
--      return ()
--    "p" -> do
--      forM_ rows (putStrLn . show)
--      loop
--    _ -> do
--      let
--        m :: String
--        (_, m, _) = line =~ [re|p ([0-9]+)|] :: (String, String, String)
--        numstr = drop 2 m
--        num :: Int
--        num = read numstr
--        nRows = take num rows
--      forM_ nRows (putStrLn . show)
--      loop

getKey :: IO [Char]
getKey = reverse <$> getKey' ""
  where
  getKey' chars = do
    char <- getChar
    more <- hReady stdin
    (if more then getKey' else return) (char:chars)
