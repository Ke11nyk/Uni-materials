{-# LANGUAGE OverloadedStrings #-}

module Main where

import Database.MySQL.Simple
import qualified Data.Text as T
import Data.Time
import Data.Int

-- Типи даних
data Author = Author
    { authorId :: Int64
    , firstName :: T.Text
    , lastName :: T.Text
    , email :: T.Text
    , department :: T.Text
    } deriving (Show)

data Article = Article
    { articleId :: Int64
    , title :: T.Text
    , authorIdRef :: Int64
    , annotation :: T.Text
    , filePath :: T.Text
    , publicationDate :: UTCTime
    , category :: T.Text
    , isPublished :: Bool
    } deriving (Show)

-- Конфігурація бази даних
dbConfig :: ConnectInfo
dbConfig = defaultConnectInfo
    { connectHost = "localhost"
    , connectPort = 3306
    , connectUser = "root"
    , connectPassword = "password"
    , connectDatabase = "faculty_newspaper"
    }

-- Головна функція
main :: IO ()
main = do
    putStrLn "=== Запуск системи 'Мережева газета факультету' ==="
    putStrLn "\nПідключення до бази даних..."
    
    conn <- connect dbConfig
    putStrLn "✓ Підключено успішно!"
    
    -- Отримуємо авторів
    putStrLn "\n--- Список авторів ---"
    authors <- query_ conn "SELECT author_id, first_name, last_name, email, department FROM authors" :: IO [(Int64, T.Text, T.Text, T.Text, T.Text)]
    
    if null authors
        then putStrLn "Авторів не знайдено. Створюємо тестові дані..."
        else do
            mapM_ (\(aid, fname, lname, _, dept) ->
                putStrLn $ show aid ++ ". " ++ T.unpack fname ++ " " ++ T.unpack lname ++ " (" ++ T.unpack dept ++ ")")
                authors
    
    -- Отримуємо статті
    putStrLn "\n--- Список статей ---"
    articles <- query_ conn "SELECT article_id, title, category, is_published FROM articles" :: IO [(Int64, T.Text, T.Text, Bool)]
    
    if null articles
        then putStrLn "Статей не знайдено."
        else do
            mapM_ (\(aid, ttl, cat, pub) -> 
                putStrLn $ show aid ++ ". " ++ T.unpack ttl ++ " [" ++ T.unpack cat ++ "] " ++ if pub then "(Опубліковано)" else "(Чернетка)")
                articles
    
    -- Статистика
    putStrLn "\n--- Статистика системи ---"
    authorCountResult <- query_ conn "SELECT COUNT(*) FROM authors" :: IO [Only Int]
    articleCountResult <- query_ conn "SELECT COUNT(*) FROM articles" :: IO [Only Int]
    readerCountResult <- query_ conn "SELECT COUNT(*) FROM readers" :: IO [Only Int]
    
    let [Only authorCount] = authorCountResult
        [Only articleCount] = articleCountResult
        [Only readerCount] = readerCountResult
    
    putStrLn $ "Авторів: " ++ show authorCount
    putStrLn $ "Статей: " ++ show articleCount
    putStrLn $ "Читачів: " ++ show readerCount

    close conn
    putStrLn "\n=== Система завершила роботу ==="
