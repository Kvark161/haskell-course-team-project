{-# LANGUAGE OverloadedStrings #-}

module Storage (
    acceptTodo,
    findByTitle,
    findByDescription,
    getAll, 
    getById, 
    insertTodo
)where


import Database.MySQL.Base
import GHC.Word
import Data.Int
import qualified System.IO.Streams as Streams
import qualified Data.Text as T

import Todo
import Utils
    
toTodo :: [MySQLValue] -> Todo
toTodo [MySQLInt32 id, MySQLText title, MySQLText desc, MySQLDateTime at, MySQLNull] = Todo (fromIntegral id) (T.unpack title) (T.unpack desc) (at) Nothing
toTodo [MySQLInt32 id, MySQLText title, MySQLText desc, MySQLDateTime at, MySQLDateTime et] = Todo (fromIntegral id) (T.unpack title) (T.unpack desc) at (Just et)

getConnection = connect defaultConnectInfoMB4 {
                            ciUser = "root",
                        ciPassword = "mysql",
                        ciDatabase = "todolist"}
    
getAll :: IO [Todo]
getAll = do
    conn <- getConnection
    (defs, is) <- query_ conn "select * from todos"
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

acceptTodo :: GHC.Word.Word32 -> IO ()
acceptTodo id = do
    conn <- getConnection
    s <- prepareStmt conn "UPDATE todos SET end_date = ? WHERE id = ?"
    t <- getCurrentLocalTime
    res <- executeStmt conn s [MySQLDateTime t, MySQLInt32U id]
    close conn
    
    
