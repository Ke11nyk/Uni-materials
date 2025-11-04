{-# LANGUAGE OverloadedStrings #-}

module Operations.Materials
    ( getAllMaterials
    , displayMaterials
    , addMaterial
    , linkMaterialToArticle
    , unlinkMaterialFromArticle
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
            
            -- Перевіряємо чи зв'язаний з статтями
            [Only articleCount] <- query conn
                "SELECT COUNT(*) FROM article_materials WHERE material_id = ?"
                (Only $ materialId m) :: IO [Only Int]
            
            putStrLn $ show (materialId m) ++ ". " ++ T.unpack (materialTitle m)
            putStrLn $ "   Автор: " ++ authorName
            putStrLn $ "   Тип: " ++ show (materialType m)
            putStrLn $ "   Файл: " ++ T.unpack (materialFilePath m)
            case materialDescription m of
                Just desc -> putStrLn $ "   Опис: " ++ T.unpack desc
                Nothing -> return ()
            putStrLn $ "   Зв'язано зі статтями: " ++ show articleCount
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
            Just authId -> do
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
                    Just matType -> do
                        desc <- promptText "Опис (Enter - пропустити): "
                        let descValue = if T.null desc then Nothing else Just desc
                        
                        void $ execute conn 
                            "INSERT INTO graphic_materials (title, author_id, file_path, material_type, description) \
                            \VALUES (?, ?, ?, ?, ?)"
                            (title, authId, filepath, materialTypeToText matType, descValue)
                        
                        [Only newMaterialId] <- query_ conn "SELECT LAST_INSERT_ID()" :: IO [Only Int64]
                        
                        putStrLn "\n✓ Матеріал успішно додано!"
                        
                        -- Запитуємо чи зв'язати зі статтею
                        linkStr <- prompt "\nЗв'язати зі статтею зараз? (y/n): "
                        when (linkStr == "y" || linkStr == "Y") $ do
                            linkMaterialToArticle conn newMaterialId
                        
                    Nothing -> putStrLn "\n✗ Невірний тип матеріалу!"
            Nothing -> putStrLn "\n✗ Невірний ID автора!"

-- | Зв'язати матеріал зі статтею
linkMaterialToArticle :: Connection -> Int64 -> IO ()
linkMaterialToArticle conn matId = do
    clearScreen
    
    -- Показати всі статті
    articles <- query_ conn
        "SELECT article_id, title, is_published FROM articles ORDER BY publication_date DESC"
        :: IO [(Int64, T.Text, Bool)]
    
    printSeparator
    putStrLn "СПИСОК СТАТЕЙ"
    printSeparator
    
    if null articles
        then putStrLn "Немає статей для зв'язку."
        else do
            forM_ articles $ \(aid, title, published) -> do
                let status = if published then "[Опубліковано]" else "[Чернетка]"
                putStrLn $ show aid ++ ". " ++ T.unpack title ++ " " ++ status
            
            putStrLn ""
            articleIdStr <- prompt "Введіть ID статті (або 0 для скасування): "
            
            case readMaybe articleIdStr :: Maybe Int64 of
                Just 0 -> putStrLn "Скасовано."
                Just aid -> do
                    -- Перевіряємо чи вже зв'язано
                    existing <- query conn
                        "SELECT COUNT(*) FROM article_materials WHERE article_id = ? AND material_id = ?"
                        (aid, matId) :: IO [Only Int]
                    
                    case existing of
                        [Only count] | count > 0 -> 
                            putStrLn "\n⚠ Цей матеріал вже зв'язаний з цією статтею!"
                        _ -> do
                            void $ execute conn
                                "INSERT INTO article_materials (article_id, material_id) VALUES (?, ?)"
                                (aid, matId)
                            putStrLn "\n✓ Матеріал зв'язано зі статтею!"
                Nothing -> putStrLn "\n✗ Невірний ID статті!"

-- | Розв'язати матеріал від статті
unlinkMaterialFromArticle :: Connection -> Int64 -> Int64 -> IO ()
unlinkMaterialFromArticle conn artId matId = do
    void $ execute conn
        "DELETE FROM article_materials WHERE article_id = ? AND material_id = ?"
        (artId, matId)
    putStrLn "✓ Матеріал відв'язано від статті!"

-- | Отримати матеріали статті
getArticleMaterials :: Connection -> Int64 -> IO [GraphicMaterial]
getArticleMaterials conn artId = do
    results <- query conn
        "SELECT gm.material_id, gm.title, gm.author_id, gm.file_path, gm.material_type, gm.description \
        \FROM graphic_materials gm \
        \JOIN article_materials am ON gm.material_id = am.material_id \
        \WHERE am.article_id = ?"
        (Only artId) :: IO [(Int64, T.Text, Int64, T.Text, T.Text, Maybe T.Text)]
    
    return [GraphicMaterial mid title authId path mtype desc (Just artId) | 
            (mid, title, authId, path, mtypeText, desc) <- results,
            Just mtype <- [textToMaterialType mtypeText]]

-- | Відобразити матеріали статті
displayArticleMaterials :: Connection -> Int64 -> IO ()
displayArticleMaterials conn artId = do
    materials <- getArticleMaterials conn artId
    if null materials
        then putStrLn "   Матеріали: —"
        else do
            putStrLn "   Матеріали:"
            forM_ materials $ \m ->
                putStrLn $ "     • " ++ T.unpack (materialTitle m) ++ 
                          " (" ++ show (materialType m) ++ ")"
