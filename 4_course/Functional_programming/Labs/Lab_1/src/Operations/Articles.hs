{-# LANGUAGE OverloadedStrings #-}

module Operations.Articles
    ( getAllArticles
    , displayArticles
    , addArticle
    , updateArticle
    , deleteArticle
    ) where

import Database.MySQL.Simple
import qualified Data.Text as T
import Data.Int
import Control.Monad (forM_, when, void)
import Text.Read (readMaybe)

import Models.Types
import UI.Utils
import Operations.Authors (getAllAuthors, displayAuthors)
import Operations.Categories (getAllCategories, displayCategories, getCategoryName)
import Operations.Tags (getAllTags, displayTags, linkArticleTag, displayArticleTags)

-- | Отримати всі статті з БД
getAllArticles :: Connection -> IO [Article]
getAllArticles conn = do
    results <- query_ conn 
        "SELECT article_id, title, author_id, annotation, file_path, category_id, is_published \
        \FROM articles ORDER BY publication_date DESC"
        :: IO [(Int64, T.Text, Int64, T.Text, T.Text, Int64, Bool)]
    return [Article aid title authId annot path catId pub | 
            (aid, title, authId, annot, path, catId, pub) <- results]

-- | Відобразити список статей
displayArticles :: Connection -> [Article] -> IO ()
displayArticles conn articles = do
    printSeparator
    putStrLn "СПИСОК СТАТЕЙ"
    printSeparator
    if null articles
        then putStrLn "Немає статей."
        else forM_ articles $ \a -> do
            -- Отримуємо ім'я автора
            authorResults <- query conn 
                "SELECT first_name, last_name FROM authors WHERE author_id = ?" 
                (Only $ articleAuthorId a) :: IO [(T.Text, T.Text)]
            
            let authorName = case authorResults of
                    [(fname, lname)] -> T.unpack fname ++ " " ++ T.unpack lname
                    _ -> "Невідомий автор"
            
            -- Отримуємо назву категорії
            categoryName <- getCategoryName conn (articleCategoryId a)
            
            putStrLn $ show (articleId a) ++ ". " ++ T.unpack (articleTitle a)
            putStrLn $ "   Автор: " ++ authorName
            putStrLn $ "   Категорія: " ++ T.unpack categoryName
            putStrLn $ "   Опубліковано: " ++ (if articleIsPublished a then "Так" else "Ні")
            
            -- Відобразити теги
            displayArticleTags conn (articleId a)
            putStrLn ""

-- | Додати нову статтю
addArticle :: Connection -> IO ()
addArticle conn = do
    clearScreen
    authors <- getAllAuthors conn
    displayAuthors authors
    
    when (not $ null authors) $ do
        authorIdStr <- prompt "\nВведіть ID автора: "
        case readMaybe authorIdStr :: Maybe Int64 of
            Just authorId -> do
                title <- promptText "Назва статті: "
                annot <- promptText "Анотація: "
                filepath <- promptText "Шлях до файлу: "
                
                -- Вибір категорії
                categories <- getAllCategories conn
                displayCategories categories
                categoryIdStr <- prompt "Введіть ID категорії: "
                
                case readMaybe categoryIdStr :: Maybe Int64 of
                    Just categoryId -> do
                        publishedStr <- prompt "Опублікувати? (y/n): "
                        let isPublished = publishedStr == "y" || publishedStr == "Y"
                        
                        -- Вставка статті
                        void $ execute conn 
                            "INSERT INTO articles (title, author_id, annotation, file_path, category_id, is_published) \
                            \VALUES (?, ?, ?, ?, ?, ?)"
                            (title, authorId, annot, filepath, categoryId, isPublished)
                        
                        -- Отримуємо ID нової статті
                        [Only newArticleId] <- query_ conn "SELECT LAST_INSERT_ID()" :: IO [Only Int64]
                        
                        -- Створюємо запис статистики
                        void $ execute conn 
                            "INSERT INTO article_statistics (article_id) VALUES (?)"
                            (Only newArticleId)
                        
                        -- Додавання тегів
                        addTagsStr <- prompt "Додати теги? (y/n): "
                        when (addTagsStr == "y" || addTagsStr == "Y") $ do
                            addTagsToArticle conn newArticleId
                        
                        putStrLn "\n✓ Статтю успішно додано!"
                    Nothing -> putStrLn "\n✗ Невірний ID категорії!"
            Nothing -> putStrLn "\n✗ Невірний ID автора!"

-- | Додати теги до статті
addTagsToArticle :: Connection -> Int64 -> IO ()
addTagsToArticle conn articleId = do
    tags <- getAllTags conn
    displayTags tags
    putStrLn "\nВведіть ID тегів через кому (наприклад: 1,3,5) або Enter для пропуску:"
    tagIdsStr <- prompt "> "
    
    let tagIds = parseTagIds tagIdsStr
    forM_ tagIds $ \tagId -> linkArticleTag conn articleId tagId
    
    when (not $ null tagIds) $
        putStrLn "✓ Теги додано!"

-- | Парсинг ID тегів з рядка
parseTagIds :: String -> [Int64]
parseTagIds str = 
    let parts = map (readMaybe . filter (/= ' ')) (split ',' str)
    in [x | Just x <- parts]
  where
    split :: Char -> String -> [String]
    split _ [] = []
    split c s = let (first, rest) = break (== c) s
                in first : case rest of
                    [] -> []
                    (_:rs) -> split c rs

-- | Оновити статтю
updateArticle :: Connection -> IO ()
updateArticle conn = do
    clearScreen
    articles <- getAllArticles conn
    displayArticles conn articles
    
    when (not $ null articles) $ do
        articleIdStr <- prompt "\nВведіть ID статті для редагування: "
        case readMaybe articleIdStr :: Maybe Int64 of
            Just aid -> do
                title <- promptText "Нова назва (Enter - пропустити): "
                annot <- promptText "Нова анотація (Enter - пропустити): "
                
                when (not $ T.null title) $
                    void $ execute conn "UPDATE articles SET title = ? WHERE article_id = ?" (title, aid)
                when (not $ T.null annot) $
                    void $ execute conn "UPDATE articles SET annotation = ? WHERE article_id = ?" (annot, aid)
                
                -- Зміна категорії
                changeCatStr <- prompt "Змінити категорію? (y/n): "
                when (changeCatStr == "y" || changeCatStr == "Y") $ do
                    categories <- getAllCategories conn
                    displayCategories categories
                    catIdStr <- prompt "Введіть новий ID категорії: "
                    case readMaybe catIdStr :: Maybe Int64 of
                        Just catId -> void $ execute conn 
                            "UPDATE articles SET category_id = ? WHERE article_id = ?" (catId, aid)
                        Nothing -> putStrLn "Невірний ID категорії"
                
                -- Зміна статусу публікації
                pubStr <- prompt "Змінити статус публікації? (y/n): "
                when (pubStr == "y" || pubStr == "Y") $ do
                    newStatus <- prompt "Опубліковано? (y/n): "
                    let isPublished = newStatus == "y" || newStatus == "Y"
                    void $ execute conn "UPDATE articles SET is_published = ? WHERE article_id = ?" 
                        (isPublished, aid)
                
                putStrLn "\n✓ Статтю оновлено!"
            Nothing -> putStrLn "\n✗ Невірний ID!"

-- | Видалити статтю
deleteArticle :: Connection -> IO ()
deleteArticle conn = do
    clearScreen
    articles <- getAllArticles conn
    displayArticles conn articles
    
    when (not $ null articles) $ do
        articleIdStr <- prompt "\nВведіть ID статті для видалення: "
        case readMaybe articleIdStr :: Maybe Int64 of
            Just aid -> do
                void $ execute conn "DELETE FROM articles WHERE article_id = ?" (Only aid)
                putStrLn "\n✓ Статтю видалено!"
            Nothing -> putStrLn "\n✗ Невірний ID!"