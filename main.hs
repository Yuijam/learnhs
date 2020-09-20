import Control.Monad
import Data.List
import System.Directory
import System.Environment
import System.IO

data Status = Unfinished | Finished deriving (Show, Read)

data Item = Item {itemId :: String, text :: String, status :: Status}

instance Show Item where
  show (Item itemId text status) = itemId ++ ". " ++ text ++ "  #" ++ show status

filepath = "todo.txt"

newId contents
  | null contents = "1"
  | otherwise = show $ (read $ head $ split '|' $ last $ lines contents) + 1

getNewContents oldContents newItem
  | null oldContents = newItem
  | otherwise = oldContents ++ newItem

toDBItem :: Item -> String
toDBItem item = (itemId item) ++ "|" ++ (text item) ++ "|" ++ (show $ status item)

toObjItem :: String -> Item
toObjItem dbItem =
  let [itemId, text, status] = split '|' dbItem
   in Item {itemId = itemId, text = text, status = (read status)}

split :: Eq a => a -> [a] -> [[a]]
split d [] = []
split d s = x : split d (drop 1 y) where (x, y) = span (/= d) s

modify :: (String -> String) -> IO ()
modify f = do
  handle <- openFile filepath ReadWriteMode
  (tempName, tempHandle) <- openTempFile "." "temp"
  contents <- hGetContents handle
  let newContents = f contents
  hPutStr tempHandle newContents
  hClose handle
  hClose tempHandle
  removeFile filepath
  renameFile tempName filepath

add args = do
  case args of
    [] -> putStrLn "you should input something to add, like 'add learn haskell'"
    args -> do
      let text = intercalate " " args
          addItem text newItemId = Item newItemId text Unfinished
          combineFun h f g p = h $ f p $ g p
          f = combineFun (++ "\n") getNewContents (toDBItem . addItem text . newId)
      modify f

tryAddSomethingMsgText = "Here is Nothing, try use 'add something'"

toItemsStr contents
  | null contents = tryAddSomethingMsgText
  | otherwise = unlines $ map (show . toObjItem) $ lines contents

view _ = do
  isExist <- doesFileExist filepath
  case isExist of
    True -> do
      contents <- readFile filepath
      putStrLn $ toItemsStr contents
    False -> putStrLn tryAddSomethingMsgText

parseItem contentLine =
  let splitedLine = words contentLine
      itemId = take 1 $ head splitedLine
      text = intercalate " " $ drop 1 splitedLine
   in Item {itemId = itemId, text = text, status = Unfinished}

remove args = do
  case args of
    [] -> putStrLn "need id to remove"
    (removeId : _) -> modify $ unlines . filter ((removeId /=) . itemId . toObjItem) . lines

updateItem updateId updateText item
  | itemId item == updateId = Item updateId updateText (status item)
  | otherwise = item

update :: [[Char]] -> IO ()
update args = do
  case args of
    [] -> putStrLn "need id and text to update"
    [updateId] -> putStrLn "need text to update"
    (updateId : textArray) -> do
      let text = intercalate " " textArray
          f = unlines . map (toDBItem . updateItem updateId text . toObjItem) . lines
      modify f

finishItem finishedId item
  | finishedId == itemId item = Item finishedId (text item) Finished
  | otherwise = item

finish [finishedId] = do
  let f = unlines . map (toDBItem . finishItem finishedId . toObjItem) . lines
  modify f

dispatch = [("add", add), ("view", view), ("remove", remove), ("update", update), ("finish", finish)]

commandNotFoundMsg = "command is not exist, try 'add something', 'view', 'update id something', 'finish id'"

main = forever $ do
  input <- getLine
  case words input of
    [] -> putStrLn "you should input something"
    (command : args) -> do
      case lookup command dispatch of
        Just action -> action args
        Nothing -> putStrLn commandNotFoundMsg