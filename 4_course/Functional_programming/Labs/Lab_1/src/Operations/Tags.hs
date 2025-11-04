{-# LANGUAGE OverloadedStrings #-}

module Operations.Tags
    ( getAllTags
    , displayTags
    , addTag
    , linkArticleTag
    , getArticleTags
    , displayArticleTags
    ) where

import Database.MySQL.Simple
import qualified Data.Text as T
import Data.Int
import Control.Monad (forM_, void)
import Text.Read (readMaybe)

import Models.Types
import UI.Utils

-- | Отримати всі теги
getAllTags :: Connection -> IO [Tag]
getAllTags conn = do
    results <- query_ conn 
        "SELECT tag_id, tag_name, usage_count FROM tags ORDER BY tag_name" 
        :: IO [(Int64, T.Text, Int64)]
    return [Tag tid tname tcount | (tid, tname, tcount) <- results]

-- | Відобразити список тегів
displayTags :: [Tag] -> IO ()
displayTags tags = do
    printSeparator
    putStrLn "СПИСОК ТЕГІВ"
    printSeparator
    if null tags
        then putStrLn "Немає тегів."
        else forM_ tags $ \t -> do
            putStrLn $ show (tagId t) ++ ". " ++ T.unpack (tagName t) ++ 
                       " (використано: " ++ show (tagUsageCount t) ++ " разів)"

-- | Додати новий тег
addTag :: Connection -> IO ()
addTag conn = do
    clearScreen
    printSeparator
    putStrLn "ДОДАВАННЯ НОВОГО ТЕГУ"
    printSeparator
    
    name <- promptText "Назва тегу: "
    
    void $ execute conn 
        "INSERT INTO tags (tag_name) VALUES (?)"
        (Only name)
    
    putStrLn "\n✓ Тег успішно додано!"

-- | Прив'язати тег до статті
linkArticleTag :: Connection -> Int64 -> Int64 -> IO ()
linkArticleTag conn articleId tagId = do
    void $ execute conn 
        "INSERT IGNORE INTO article_tags (article_id, tag_id) VALUES (?, ?)"
        (articleId, tagId)
    void $ execute conn 
        "UPDATE tags SET usage_count = usage_count + 1 WHERE tag_id = ?"
        (Only tagId)

-- | Отримати теги статті
getArticleTags :: Connection -> Int64 -> IO [Tag]
getArticleTags conn articleId = do
    results <- query conn 
        "SELECT t.tag_id, t.tag_name, t.usage_count FROM tags t \
        \JOIN article_tags at ON t.tag_id = at.tag_id \
        \WHERE at.article_id = ?"
        (Only articleId) :: IO [(Int64, T.Text, Int64)]
    return [Tag tid tname tcount | (tid, tname, tcount) <- results]

-- | Відобразити теги статті
displayArticleTags :: Connection -> Int64 -> IO ()
displayArticleTags conn articleId = do
    tags <- getArticleTags conn articleId
    if null tags
        then putStrLn "   Теги: —"
        else putStrLn $ "   Теги: " ++ T.unpack (T.intercalate ", " [tagName t | t <- tags])