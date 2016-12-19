{-# LANGUAGE OverloadedStrings #-}

module Storage where


import Database.MySQL.Base
import GHC.Word
import Data.Int
import qualified System.IO.Streams as Streams
import qualified Data.Text as T

import Todo
    
toTodo :: [MySQLValue] -> Todo
toTodo [MySQLInt32 id, MySQLText desc] = Todo (fromIntegral id) (T.unpack desc)

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

getById :: GHC.Word.Word32 -> IO Todo
getById id = do
    conn <- getConnection
    s <- prepareStmt conn "SELECT * FROM todos where id = ?"
    (defs, is) <- queryStmt conn s [MySQLInt32U id]
    close conn
    res <- Streams.toList is
    return $ toTodo $ head res

insertTodo :: T.Text -> IO Todo
insertTodo desc = do
    conn <- getConnection
    result <- execute conn "insert into todos (description) values (?)" [MySQLText desc]
    let id = okLastInsertID result
    s <- prepareStmt conn "SELECT * FROM todos where id = ?"
    (defs, is) <- queryStmt conn s [MySQLInt32U (fromIntegral (id :: Int))]
    close conn
    res <- Streams.toList is
    return $ toTodo $ head res
