{-# LANGUAGE OverloadedStrings #-}

module Operations.Authors
    ( getAllAuthors
    , displayAuthors
    , addAuthor
    , updateAuthor
    , deleteAuthor
    ) where

import Database.MySQL.Simple
import qualified Data.Text as T
import Data.Int
import Control.Monad (forM_, when, void)
import Text.Read (readMaybe)

import Models.Types
import UI.Utils

-- | Отримати всіх авторів з БД
getAllAuthors :: Connection -> IO [Author]
getAllAuthors conn = do
    results <- query_ conn 
        "SELECT author_id, first_name, last_name, email, department FROM authors ORDER BY last_name" 
        :: IO [(Int64, T.Text, T.Text, T.Text, T.Text)]
    return [Author aid fname lname email dept | (aid, fname, lname, email, dept) <- results]

-- | Відобразити список авторів
displayAuthors :: [Author] -> IO ()
displayAuthors authors = do
    printSeparator
    putStrLn "СПИСОК АВТОРІВ"
    printSeparator
    if null authors
        then putStrLn "Немає зареєстрованих авторів."
        else forM_ authors $ \a -> do
            putStrLn $ show (authorId a) ++ ". " ++ 
                       T.unpack (authorFirstName a) ++ " " ++ 
                       T.unpack (authorLastName a)
            putStrLn $ "   Email: " ++ T.unpack (authorEmail a)
            putStrLn $ "   Кафедра: " ++ T.unpack (authorDepartment a)
            putStrLn ""

-- | Додати нового автора
addAuthor :: Connection -> IO ()
addAuthor conn = do
    clearScreen
    printSeparator
    putStrLn "ДОДАВАННЯ НОВОГО АВТОРА"
    printSeparator
    
    fname <- promptText "Ім'я: "
    lname <- promptText "Прізвище: "
    email <- promptText "Email: "
    dept <- promptText "Кафедра: "
    
    void $ execute conn 
        "INSERT INTO authors (first_name, last_name, email, department) VALUES (?, ?, ?, ?)"
        (fname, lname, email, dept)
    
    putStrLn "\n✓ Автора успішно додано!"

-- | Оновити дані автора
updateAuthor :: Connection -> IO ()
updateAuthor conn = do
    clearScreen
    authors <- getAllAuthors conn
    displayAuthors authors
    
    when (not $ null authors) $ do
        authorIdStr <- prompt "\nВведіть ID автора для редагування: "
        case readMaybe authorIdStr :: Maybe Int64 of
            Just aid -> do
                fname <- promptText "Нове ім'я (Enter - пропустити): "
                lname <- promptText "Нове прізвище (Enter - пропустити): "
                dept <- promptText "Нова кафедра (Enter - пропустити): "
                
                when (not $ T.null fname) $
                    void $ execute conn "UPDATE authors SET first_name = ? WHERE author_id = ?" (fname, aid)
                when (not $ T.null lname) $
                    void $ execute conn "UPDATE authors SET last_name = ? WHERE author_id = ?" (lname, aid)
                when (not $ T.null dept) $
                    void $ execute conn "UPDATE authors SET department = ? WHERE author_id = ?" (dept, aid)
                
                putStrLn "\n✓ Дані автора оновлено!"
            Nothing -> putStrLn "\n✗ Невірний ID!"

-- | Видалити автора
deleteAuthor :: Connection -> IO ()
deleteAuthor conn = do
    clearScreen
    authors <- getAllAuthors conn
    displayAuthors authors
    
    when (not $ null authors) $ do
        authorIdStr <- prompt "\nВведіть ID автора для видалення: "
        case readMaybe authorIdStr :: Maybe Int64 of
            Just aid -> do
                void $ execute conn "DELETE FROM authors WHERE author_id = ?" (Only aid)
                putStrLn "\n✓ Автора видалено!"
            Nothing -> putStrLn "\n✗ Невірний ID!"