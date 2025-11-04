{-# LANGUAGE OverloadedStrings #-}

module Operations.Comments
    ( addComment
    , viewComments
    ) where

import Database.MySQL.Simple
import qualified Data.Text as T
import Data.Int
import Control.Monad (forM_, when, void)
import Text.Read (readMaybe)

import Models.Types
import UI.Utils
import Operations.Articles (getAllArticles, displayArticles)

-- | Додати коментар до статті
addComment :: Connection -> IO ()
addComment conn = do
    clearScreen
    
    -- Простий вибір читача
    putStrLn "Введіть ваш ID читача (для тестування):"
    readerIdStr <- prompt "> "
    
    case readMaybe readerIdStr :: Maybe Int64 of
        Just readerId -> do
            articles <- getAllArticles conn
            displayArticles conn articles
            
            when (not $ null articles) $ do
                articleIdStr <- prompt "\nВведіть ID статті для коментування: "
                
                case readMaybe articleIdStr :: Maybe Int64 of
                    Just aid -> do
                        commentText <- promptText "Ваш коментар: "
                        ratingStr <- prompt "Оцінка (1-5, Enter - без оцінки): "
                        
                        let rating = readMaybe ratingStr :: Maybe Int
                        void $ execute conn 
                            "INSERT INTO comments (article_id, reader_id, comment_text, rating) VALUES (?, ?, ?, ?)"
                            (aid, readerId, commentText, rating)
                        
                        -- Оновлюємо статистику
                        void $ execute conn 
                            "UPDATE article_statistics SET comment_count = comment_count + 1 WHERE article_id = ?"
                            (Only aid)
                        
                        when (rating /= Nothing) $ do
                            void $ execute conn 
                                "UPDATE article_statistics SET average_rating = \
                                \(SELECT AVG(rating) FROM comments WHERE article_id = ? AND rating IS NOT NULL) \
                                \WHERE article_id = ?"
                                (aid, aid)
                        
                        putStrLn "\n✓ Коментар додано!"
                    Nothing -> putStrLn "\n✗ Невірний ID статті!"
        Nothing -> putStrLn "\n✗ Невірний ID читача!"

-- | Переглянути коментарі до статті
viewComments :: Connection -> IO ()
viewComments conn = do
    clearScreen
    articles <- getAllArticles conn
    displayArticles conn articles
    
    when (not $ null articles) $ do
        articleIdStr <- prompt "\nВведіть ID статті для перегляду коментарів: "
        case readMaybe articleIdStr :: Maybe Int64 of
            Just aid -> do
                comments <- query conn 
                    "SELECT c.comment_id, c.comment_text, c.rating, r.first_name, r.last_name \
                    \FROM comments c JOIN readers r ON c.reader_id = r.reader_id \
                    \WHERE c.article_id = ? ORDER BY c.comment_date DESC"
                    (Only aid) :: IO [(Int64, T.Text, Maybe Int, T.Text, T.Text)]
                
                printSeparator
                putStrLn "КОМЕНТАРІ"
                printSeparator
                if null comments
                    then putStrLn "Коментарів поки немає."
                    else forM_ comments $ \(cid, text, rating, fname, lname) -> do
                        putStrLn $ T.unpack fname ++ " " ++ T.unpack lname
                        putStrLn $ "Оцінка: " ++ maybe "—" show rating
                        putStrLn $ T.unpack text
                        putStrLn ""
            Nothing -> putStrLn "\n✗ Невірний ID!"