{-# LANGUAGE OverloadedStrings #-}

module Storage (
    acceptTodo,
    findByTitle,
    findByDescription,
    findByDate,
    getAll,
    getAllDone, 
    getAllNotDone,
    getById, 
    deleteById,
    insertTodo
)where


import Database.MySQL.Base
import GHC.Word
import Data.Int
import qualified System.IO.Streams as Streams
import qualified Data.Text as T
import Data.Pool

import Todo
import Utils

toTodo :: [MySQLValue] -> Todo
toTodo [MySQLInt32 id, MySQLText title, MySQLText desc, MySQLDateTime at, MySQLNull] = Todo (fromIntegral id) (T.unpack title) (T.unpack desc) (at) Nothing
toTodo [MySQLInt32 id, MySQLText title, MySQLText desc, MySQLDateTime at, MySQLDateTime et] = Todo (fromIntegral id) (T.unpack title) (T.unpack desc) at (Just et)

getConnection = connect defaultConnectInfoMB4 {
                            ciUser = "root",
                        ciPassword = "mysql",
                        ciDatabase = "todolist"}


getAll' conn = do
    (defs, is) <- query_ conn "select * from todos"
    res <- Streams.toList is
    return $ map toTodo res

getAll pool = withResource pool getAll'

getAllDone :: IO [Todo]
getAllDone = do
    conn <- getConnection
    (defs, is) <- query_ conn "select * from todos where end_date is NOT NULL"
    close conn
    res <- Streams.toList is
    return $ map toTodo res

getAllNotDone :: IO [Todo]
getAllNotDone = do
    conn <- getConnection
    (defs, is) <- query_ conn "select * from todos where end_date is NULL"
    close conn
    res <- Streams.toList is
    return $ map toTodo res

getById :: GHC.Word.Word32 -> IO (Maybe Todo)
getById id = do
    conn <- getConnection
    s <- prepareStmt conn "SELECT * FROM todos where id = ?"
    (defs, is) <- queryStmt conn s [MySQLInt32U id]
    close conn
    res <- Streams.toList is
    return $ (if null res then Nothing else Just (toTodo $ head res))
    
deleteById :: GHC.Word.Word32 -> IO (String)
deleteById id = do
    conn <- getConnection
    query <- getById id
    case query of
       Nothing -> return "Nothing was deleted, because there is no task with such id in DB"
       Just _  -> do  result <- execute conn "DELETE FROM todos where id = ?" [MySQLInt32U id]
                      close conn
                      return $ "Task with id "++(show id)++" was deleted"

insertTodo :: Todo -> IO Todo
insertTodo obj = do
    conn <- getConnection
    result <- execute conn "insert into todos (title, description, add_date) values (?,?, ?)" [MySQLText (T.pack (title obj)), MySQLText (T.pack (desc obj)), MySQLDateTime (add_date obj)]
    let id = okLastInsertID result
    s <- prepareStmt conn "SELECT * FROM todos where id = ?"
    (defs, is) <- queryStmt conn s [MySQLInt32U (fromIntegral (id :: Int))]
    close conn
    res <- Streams.toList is
    return $ toTodo $ head res
    
findByTitle :: String -> IO [Todo]
findByTitle title = do
    conn <- getConnection
    s <- prepareStmt conn "SELECT * FROM todos where title = ?"
    (defs, is) <- queryStmt conn s [MySQLText (T.pack title)]
    close conn
    res <- Streams.toList is
    return $ map toTodo res
    
findByDescription :: String -> IO [Todo]
findByDescription description = do
    conn <- getConnection
    s <- prepareStmt conn "SELECT * FROM todos where description = ?"
    (defs, is) <- queryStmt conn s [MySQLText (T.pack description)]
    close conn
    res <- Streams.toList is
    return $ map toTodo res    
    
findByDate :: String -> String -> String -> IO [Todo]
findByDate year month date = do
    conn <- getConnection
    s <- prepareStmt conn "SELECT * FROM todos where YEAR(add_date) = ? && MONTH(add_date)=? && DAY(add_date)=?"
    (defs, is) <- queryStmt conn s [MySQLInt32 (read year), MySQLInt32 (read month),MySQLInt32 (read date) ]
    close conn
    res <- Streams.toList is
    return $ map toTodo res        

acceptTodo :: GHC.Word.Word32 -> IO ()
acceptTodo id = do
    conn <- getConnection
    s <- prepareStmt conn "UPDATE todos SET end_date = ? WHERE id = ?"
    t <- getCurrentLocalTime
    res <- executeStmt conn s [MySQLDateTime t, MySQLInt32U id]
    close conn
    
    
