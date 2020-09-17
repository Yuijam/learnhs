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
  let text = intercalate " " args
      addItem text newItemId = Item newItemId text Unfinished
      combineFun h f g p = h $ f p $ g p
      f = combineFun (++ "\n") getNewContents (toDBItem . addItem text . newId)
  modify f

toItemsStr = unlines . map (show . toObjItem) . lines

view _ = do
  isExist <- doesFileExist filepath
  if isExist
    then do
      contents <- readFile filepath
      if null contents
        then do
          putStrLn "Here is Nothing"
        else do
          putStrLn $ toItemsStr contents
    else do
      putStrLn "Here is Nothing"

parseItem contentLine =
  let splitedLine = words contentLine
      itemId = take 1 $ head splitedLine
      text = intercalate " " $ drop 1 splitedLine
   in Item {itemId = itemId, text = text, status = Unfinished}

remove [removeId] = do
  modify $ unlines . filter ((removeId /=) . itemId . toObjItem) . lines

updateItem updateId updateText item
  | itemId item == updateId = Item updateId updateText (status item)
  | otherwise = item

update args = do
  let (updateId : textArray) = args
      text = intercalate " " textArray
      f = unlines . map (toDBItem . updateItem updateId text . toObjItem) . lines
  modify f

finishItem finishedId item
  | finishedId == itemId item = Item finishedId (text item) Finished
  | otherwise = item

finish [finishedId] = do
  let f = unlines . map (toDBItem . finishItem finishedId . toObjItem) . lines
  modify f

dispatch = [("add", add), ("view", view), ("remove", remove), ("update", update), ("finish", finish)]

main = forever $ do
  input <- getLine
  let (command : args) = words input
      (Just action) = lookup command dispatch
  action args