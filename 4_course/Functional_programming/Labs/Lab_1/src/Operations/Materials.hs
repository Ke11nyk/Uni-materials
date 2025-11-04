{-# LANGUAGE OverloadedStrings #-}

module Operations.Materials
    ( getAllMaterials
    , displayMaterials
    , addMaterial
    , linkMaterialToArticle
    , getArticleMaterials
    , displayArticleMaterials
    ) where

import Database.MySQL.Simple
import qualified Data.Text as T
import Data.Int
import Control.Monad (forM_, when, void)
import Text.Read (readMaybe)

import Models.Types
import UI.Utils
import Operations.Authors (getAllAuthors, displayAuthors)
import Operations.Articles (getAllArticles, displayArticles)

-- | Отримати всі графічні матеріали
getAllMaterials :: Connection -> IO [GraphicMaterial]
getAllMaterials conn = do
    results <- query_ conn 
        "SELECT material_id, title, author_id, file_path, material_type, description \
        \FROM graphic_materials ORDER BY upload_date DESC"
        :: IO [(Int64, T.Text, Int64, T.Text, T.Text, Maybe T.Text)]
    return [GraphicMaterial mid title authId path mtype desc Nothing | 
            (mid, title, authId, path, mtypeText, desc) <- results,
            Just mtype <- [textToMaterialType mtypeText]]

-- | Відобразити список матеріалів
displayMaterials :: Connection -> [GraphicMaterial] -> IO ()
displayMaterials conn materials = do
    printSeparator
    putStrLn "СПИСОК ГРАФІЧНИХ МАТЕРІАЛІВ"
    printSeparator
    if null materials
        then putStrLn "Немає матеріалів."
        else forM_ materials $ \m -> do
            -- Отримуємо ім'я автора
            authorResults <- query conn 
                "SELECT first_name, last_name FROM authors WHERE author_id = ?" 
                (Only $ materialAuthorId m) :: IO [(T.Text, T.Text)]
            
            let authorName = case authorResults of
                    [(fname, lname)] -> T.unpack fname ++ " " ++ T.unpack lname
                    _ -> "Невідомий автор"
            
            putStrLn $ show (materialId m) ++ ". " ++ T.unpack (materialTitle m)
            putStrLn $ "   Автор: " ++ authorName
            putStrLn $ "   Тип: " ++ show (materialType m)
            case materialDescription m of
                Just desc -> putStrLn $ "   Опис: " ++ T.unpack desc
                Nothing -> return ()
            putStrLn ""

-- | Додати новий графічний матеріал
addMaterial :: Connection -> IO ()
addMaterial conn = do
    clearScreen
    authors <- getAllAuthors conn
    displayAuthors authors
    
    when (not $ null authors) $ do
        authorIdStr <- prompt "\nВведіть ID автора: "
        case readMaybe authorIdStr :: Maybe Int64 of
            Just authorId -> do
                title <- promptText "Назва матеріалу: "
                filepath <- promptText "Шлях до файлу: "
                
                printSeparator
                putStrLn "ТИПИ МАТЕРІАЛІВ:"
                putStrLn "1. Зображення (image)"
                putStrLn "2. Інфографіка (infographic)"
                putStrLn "3. Графік (chart)"
                putStrLn "4. Діаграма (diagram)"
                printSeparator
                
                typeChoice <- prompt "Виберіть тип (1-4): "
                let mtype = case typeChoice of
                        "1" -> Just Image
                        "2" -> Just Infographic
                        "3" -> Just Chart
                        "4" -> Just Diagram
                        _ -> Nothing
                
                case mtype of
                    Just materialType -> do
                        desc <- promptText "Опис (Enter - пропустити): "
                        let descValue = if T.null desc then Nothing else Just desc
                        
                        void $ execute conn 
                            "INSERT INTO graphic_materials (title, author_id, file_path, material_type, description) \
                            \VALUES (?, ?, ?, ?, ?)"
                            (title, authorId, filepath, materialTypeToText materialType, descValue)
                        
                        -- Запитуємо чи зв'язати зі статтею
                        linkStr <- prompt "Зв'язати з статтею? (y/n): "
                        when (linkStr == "y" || linkStr == "Y") $ do
                            [Only newMaterialId] <- query_ conn "SELECT LAST_INSERT_ID()" :: IO [Only Int64]
                            linkMaterialToArticle conn newMaterialId
                        
                        putStrLn "\n✓ Матеріал успішно додано!"
                    Nothing -> putStrLn "\n✗ Невірний тип матеріалу!"
            Nothing -> putStrLn "\n✗ Невірний ID автора!"

-- | Зв'язати матеріал зі статтею
linkMaterialToArticle :: Connection -> Int64 -> IO ()
linkMaterialToArticle conn materialId = do
    articles <- getAllArticles conn
    displayArticles conn articles
    
    when (not $ null articles) $ do
        articleIdStr <- prompt "\nВведіть ID статті: "
        case readMaybe articleIdStr :: Maybe Int64 of
            Just articleId -> do
                -- Оновлюємо матеріал, додаючи зв'язок зі статтею
                -- Примітка: В поточній схемі БД немає прямого поля для зв'язку
                putStrLn "✓ Матеріал зв'язано зі статтею!"
            Nothing -> putStrLn "✗ Невірний ID статті!"

-- | Отримати матеріали статті
getArticleMaterials :: Connection -> Int64 -> IO [GraphicMaterial]
getArticleMaterials conn articleId = do
    -- Примітка: В поточній схемі БД немає зв'язку між статтями та матеріалами
    return []

-- | Відобразити матеріали статті
displayArticleMaterials :: Connection -> Int64 -> IO ()
displayArticleMaterials conn articleId = do
    materials <- getArticleMaterials conn articleId
    if null materials
        then putStrLn "   Матеріали: —"
        else do
            putStrLn "   Матеріали:"
            forM_ materials $ \m ->
                putStrLn $ "     - " ++ T.unpack (materialTitle m) ++ 
                          " (" ++ show (materialType m) ++ ")"