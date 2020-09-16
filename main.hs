import System.Environment
import System.IO
import Data.List
import System.Directory
import Control.Monad

data Status = Unfinished | Finished deriving (Show, Read)
data Item = Item { itemId :: String, text :: String, status :: Status }
instance Show Item where
  show (Item itemId text status) = itemId ++ ". " ++ text ++ "  #" ++ show status

filepath = "todo.txt"

getFileHandle = do
  openFile filepath ReadWriteMode

newId contents 
  | null contents = "1"
  | otherwise = show $ (read $ head $ split '|' $ last $ lines contents) + 1

getNewContents oldContents newItem
  | null oldContents = newItem
  | otherwise = oldContents ++ newItem

toDBItem :: Item -> String
toDBItem item = (itemId item) ++ "|" ++ (text item) ++ "|" ++ (show $ status item)

toObjItem :: String -> Item
toObjItem dbItem = let [itemId, text, status] = split '|' dbItem
                  in Item { itemId = itemId, text = text, status = (read status)}

split :: Eq a => a -> [a] -> [[a]]
split d [] = []
split d s = x : split d (drop 1 y) where (x,y) = span (/= d) s

add args = do
  let text = intercalate " " args
  handle <- openFile filepath ReadWriteMode
  (tempName, tempHandle) <- openTempFile "." "temp"  
  contents <- hGetContents handle
  let newItemId = newId contents
  hPutStrLn tempHandle $ getNewContents contents $ toDBItem $ Item { itemId = newItemId, text = text, status = Unfinished}
  hClose handle
  hClose tempHandle
  removeFile filepath
  renameFile tempName filepath

toItemsStr :: String -> String
toItemsStr contents = let allLines = lines contents
                          objs = map (show . toObjItem) allLines
                      in unlines objs

view _ = do
  isExist <- doesFileExist filepath
  if isExist then do 
    contents <- readFile filepath
    if null contents then do putStrLn "Here is Nothing"
    else do 
      
      putStrLn $ toItemsStr contents
  else do 
    putStrLn "Here is Nothing"

parseItem contentLine = let splitedLine = words contentLine
                            itemId = take 1 $ head splitedLine
                            text = intercalate " " $ drop 1 splitedLine
                        in Item { itemId = itemId, text = text, status = Unfinished}

remove [removeId] = do
  handle <- openFile filepath ReadWriteMode
  (tempName, tempHandle) <- openTempFile "." "temp"
  contents <- hGetContents handle
  let allLines = lines contents
      newLines = filter (\line -> removeId /= ( itemId $ toObjItem line)) allLines
  hPutStr tempHandle $ unlines newLines
  hClose handle
  hClose tempHandle
  removeFile filepath
  renameFile tempName filepath  

update args = do
  let (updateId: textArray) = args
      text = intercalate " " textArray
  handle <- openFile filepath ReadWriteMode
  (tempName, tempHandle) <- openTempFile "." "temp"
  contents <- hGetContents handle
  let allLines = lines contents
      objs = map toObjItem allLines
      newObjs = map (\itemObj -> if updateId == (itemId itemObj) then Item { itemId = updateId, text = text, status = (status itemObj)} else itemObj) objs
      newLines = map toDBItem newObjs
  hPutStr tempHandle $ unlines newLines
  hClose handle
  hClose tempHandle
  removeFile filepath
  renameFile tempName filepath

finish [finishedId] = do
  handle <- openFile filepath ReadWriteMode
  (tempName, tempHandle) <- openTempFile "." "temp"
  contents <- hGetContents handle
  let allLines = lines contents
      objs = map toObjItem allLines
      newObjs = map (\itemObj -> if finishedId == (itemId itemObj) then Item { itemId = finishedId, text = (text itemObj), status = Finished} else itemObj) objs
      newLines = map toDBItem newObjs
  hPutStr tempHandle $ unlines newLines
  hClose handle
  hClose tempHandle
  removeFile filepath
  renameFile tempName filepath

dispatch = [("add", add), ("view", view), ("remove", remove), ("update", update), ("finish", finish)]

main = forever $ do
    input <- getLine
    let (command: args) = words input
        (Just action) = lookup command dispatch
    action args