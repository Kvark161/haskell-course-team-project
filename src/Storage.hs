{-# LANGUAGE OverloadedStrings #-}

module Storage (
    getConnection, getAll, getById
) where


import Database.MySQL.Base
import qualified System.IO.Streams as Streams
import qualified Data.Text as T

import Todo
    
toTodo :: [MySQLValue] -> Todo
toTodo [MySQLInt32 id, MySQLText desc] = Todo (fromIntegral id) (T.unpack desc)

getConnection = connect defaultConnectInfoMB4 {
                            ciUser = "root",
                        ciPassword = "mysql",
                        ciDatabase = "todolist"}
    
getAll conn = do
    (defs, is) <- query_ conn "select * from todos"
    res <- Streams.toList is
    return $ map toTodo res
    
getById conn id = do
    s <- prepareStmt conn "SELECT * FROM todos where id = ?"
    (defs, is) <- queryStmt conn s [MySQLInt32U id]
    res <- Streams.toList is
    return $ toTodo $ head res