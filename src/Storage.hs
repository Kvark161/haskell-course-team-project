{-# LANGUAGE OverloadedStrings #-}

module Storage (
    acceptTodo,
    findByTitle,
    findByDescription,
    findByDate,
    getAll,
    getAllDone, 
    getAllActive,
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

getAll' conn = do
    (defs, is) <- query_ conn "select * from todos"
    res <- Streams.toList is
    return $ map toTodo res

getAll pool = withResource pool getAll'

getAllDone' conn = do
    (defs, is) <- query_ conn "select * from todos where end_date is NOT NULL"
    res <- Streams.toList is
    return $ map toTodo res

getAllDone pool = withResource pool getAllDone'

getAllActive' conn = do
    (defs, is) <- query_ conn "select * from todos where end_date is NULL"
    res <- Streams.toList is
    return $ map toTodo res

getAllActive pool = withResource pool getAllActive'

getById' id conn = do
    s <- prepareStmt conn "SELECT * FROM todos where id = ?"
    (defs, is) <- queryStmt conn s [MySQLInt32U id]
    res <- Streams.toList is
    return $ (if null res then Nothing else Just (toTodo $ head res))

getById id pool = withResource pool $ getById' id

deleteById' id conn = do
    query <- getById' id conn
    case query of
       Nothing -> return "Nothing was deleted, because there is no task with such id in DB"
       Just _  -> do  result <- execute conn "DELETE FROM todos where id = ?" [MySQLInt32U id]
                      return $ "Task with id "++(show id)++" was deleted"

deleteById id pool = withResource pool $ deleteById' id

insertTodo' todo conn = do
    result <- execute conn "insert into todos (title, description, add_date) values (?,?, ?)" [MySQLText (T.pack (title todo)), MySQLText (T.pack (desc todo)), MySQLDateTime (add_date todo)]
    let id = okLastInsertID result
    s <- prepareStmt conn "SELECT * FROM todos where id = ?"
    (defs, is) <- queryStmt conn s [MySQLInt32U (fromIntegral (id :: Int))]
    res <- Streams.toList is
    return $ toTodo $ head res

insertTodo todo pool = withResource pool $ insertTodo' todo

findByTitle' title conn = do
    s <- prepareStmt conn "SELECT * FROM todos where title = ?"
    (defs, is) <- queryStmt conn s [MySQLText (T.pack title)]
    res <- Streams.toList is
    return $ map toTodo res

findByTitle todo pool = withResource pool $ findByTitle' todo

findByDescription' description conn = do
    s <- prepareStmt conn "SELECT * FROM todos where description = ?"
    (defs, is) <- queryStmt conn s [MySQLText (T.pack description)]
    res <- Streams.toList is
    return $ map toTodo res    

findByDescription description pool = withResource pool $ findByDescription' description

findByDate' year month date conn = do
    s <- prepareStmt conn "SELECT * FROM todos where YEAR(add_date) = ? && MONTH(add_date)=? && DAY(add_date)=?"
    (defs, is) <- queryStmt conn s [MySQLInt32 (read year), MySQLInt32 (read month),MySQLInt32 (read date) ]
    res <- Streams.toList is
    return $ map toTodo res        

findByDate year month date pool = withResource pool $ findByDate' year month date

acceptTodo' id conn = do
    s <- prepareStmt conn "UPDATE todos SET end_date = ? WHERE id = ?"
    t <- getCurrentLocalTime
    executeStmt conn s [MySQLDateTime t, MySQLInt32U id]

acceptTodo id pool = withResource pool $ acceptTodo' id
    
