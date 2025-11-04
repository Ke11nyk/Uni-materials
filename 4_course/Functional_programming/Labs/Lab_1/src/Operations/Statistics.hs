{-# LANGUAGE OverloadedStrings #-}

module Operations.Statistics
    ( viewStatistics
    , simulateView
    , viewCategoryStatistics
    ) where

import Database.MySQL.Simple
import qualified Data.Text as T
import Data.Int
import Control.Monad (forM_, when, void)
import Text.Read (readMaybe)

import Models.Types
import UI.Utils
import Operations.Articles (getAllArticles, displayArticles)
import Operations.Categories (getCategoryName)

-- | Переглянути загальну статистику системи
viewStatistics :: Connection -> IO ()
viewStatistics conn = do
    clearScreen
    printSeparator
    putStrLn "СТАТИСТИКА СИСТЕМИ"
    printSeparator
    
    -- Загальна статистика
    [Only authorCount] <- query_ conn "SELECT COUNT(*) FROM authors" :: IO [Only Int]
    [Only articleCount] <- query_ conn "SELECT COUNT(*) FROM articles" :: IO [Only Int]
    [Only publishedCount] <- query_ conn "SELECT COUNT(*) FROM articles WHERE is_published = TRUE" :: IO [Only Int]
    [Only readerCount] <- query_ conn "SELECT COUNT(*) FROM readers" :: IO [Only Int]
    [Only commentCount] <- query_ conn "SELECT COUNT(*) FROM comments" :: IO [Only Int]
    [Only materialCount] <- query_ conn "SELECT COUNT(*) FROM graphic_materials" :: IO [Only Int]
    [Only categoryCount] <- query_ conn "SELECT COUNT(*) FROM categories" :: IO [Only Int]
    [Only tagCount] <- query_ conn "SELECT COUNT(*) FROM tags" :: IO [Only Int]
    
    putStrLn $ "Авторів: " ++ show authorCount
    putStrLn $ "Статей: " ++ show articleCount ++ " (опубліковано: " ++ show publishedCount ++ ")"
    putStrLn $ "Читачів: " ++ show readerCount
    putStrLn $ "Коментарів: " ++ show commentCount
    putStrLn $ "Графічних матеріалів: " ++ show materialCount
    putStrLn $ "Категорій: " ++ show categoryCount
    putStrLn $ "Тегів: " ++ show tagCount
    putStrLn ""
    
    -- Топ-5 статей за переглядами
    printSeparator
    putStrLn "ТОП-5 СТАТЕЙ ЗА ПЕРЕГЛЯДАМИ"
    printSeparator
    topArticles <- query_ conn 
        "SELECT a.title, s.view_count FROM articles a \
        \JOIN article_statistics s ON a.article_id = s.article_id \
        \ORDER BY s.view_count DESC LIMIT 5"
        :: IO [(T.Text, Int64)]
    
    if null topArticles
        then putStrLn "Немає даних."
        else forM_ topArticles $ \(title, views) ->
            putStrLn $ "• " ++ T.unpack title ++ " — " ++ show views ++ " переглядів"
    
    putStrLn ""
    
    -- Топ-5 статей за рейтингом
    printSeparator
    putStrLn "ТОП-5 СТАТЕЙ ЗА РЕЙТИНГОМ"
    printSeparator
    topRated <- query_ conn 
        "SELECT a.title, s.average_rating, s.comment_count FROM articles a \
        \JOIN article_statistics s ON a.article_id = s.article_id \
        \WHERE s.average_rating > 0 \
        \ORDER BY s.average_rating DESC, s.comment_count DESC LIMIT 5"
        :: IO [(T.Text, Double, Int64)]
    
    if null topRated
        then putStrLn "Немає даних."
        else forM_ topRated $ \(title, rating, comments) ->
            putStrLn $ "• " ++ T.unpack title ++ " — " ++ 
                      show (fromIntegral (round (rating * 10)) / 10 :: Double) ++ " ★ (" ++
                      show comments ++ " коментарів)"

-- | Симулювати перегляд статті
simulateView :: Connection -> IO ()
simulateView conn = do
    clearScreen
    articles <- getAllArticles conn
    displayArticles conn articles
    
    when (not $ null articles) $ do
        articleIdStr <- prompt "\nВведіть ID статті для симуляції перегляду: "
        case readMaybe articleIdStr :: Maybe Int64 of
            Just aid -> do
                void $ execute conn 
                    "UPDATE article_statistics SET view_count = view_count + 1 WHERE article_id = ?"
                    (Only aid)
                putStrLn "\n✓ Перегляд зараховано!"
            Nothing -> putStrLn "\n✗ Невірний ID!"

-- | Переглянути статистику по категоріях
viewCategoryStatistics :: Connection -> IO ()
viewCategoryStatistics conn = do
    clearScreen
    printSeparator
    putStrLn "СТАТИСТИКА ПО КАТЕГОРІЯХ"
    printSeparator
    
    results <- query_ conn 
        "SELECT c.category_name, COUNT(a.article_id) as article_count, \
        \SUM(CASE WHEN a.is_published = TRUE THEN 1 ELSE 0 END) as published_count \
        \FROM categories c \
        \LEFT JOIN articles a ON c.category_id = a.category_id \
        \GROUP BY c.category_id, c.category_name \
        \ORDER BY article_count DESC"
        :: IO [(T.Text, Int, Int)]
    
    if null results
        then putStrLn "Немає даних."
        else forM_ results $ \(catName, total, published) ->
            putStrLn $ "• " ++ T.unpack catName ++ ": " ++ 
                      show total ++ " статей (опубліковано: " ++ show published ++ ")"
    
    putStrLn ""
    
    -- Найпопулярніші теги
    printSeparator
    putStrLn "ТОП-10 НАЙПОПУЛЯРНІШИХ ТЕГІВ"
    printSeparator
    topTags <- query_ conn 
        "SELECT tag_name, usage_count FROM tags \
        \WHERE usage_count > 0 \
        \ORDER BY usage_count DESC LIMIT 10"
        :: IO [(T.Text, Int64)]
    
    if null topTags
        then putStrLn "Немає даних."
        else forM_ topTags $ \(tagName, count) ->
            putStrLn $ "• " ++ T.unpack tagName ++ " — " ++ show count ++ " використань"