{-# LANGUAGE OverloadedStrings #-}

module UI.Menu
    ( mainMenu
    ) where

import Database.MySQL.Simple
import Data.Int (Int64)
import Text.Read (readMaybe)
import Control.Monad (when)

import UI.Utils
import qualified Operations.Authors as Authors
import qualified Operations.Articles as Articles
import qualified Operations.Categories as Categories
import qualified Operations.Tags as Tags
import qualified Operations.Materials as Materials
import qualified Operations.Comments as Comments
import qualified Operations.Statistics as Statistics

-- | Головне меню системи
mainMenu :: Connection -> IO ()
mainMenu conn = do
    clearScreen
    printSeparator
    putStrLn "МЕРЕЖЕВА ГАЗЕТА ФАКУЛЬТЕТУ"
    printSeparator
    putStrLn "АВТОРИ:"
    putStrLn "  1.  Переглянути всіх авторів"
    putStrLn "  2.  Додати автора"
    putStrLn "  3.  Редагувати автора"
    putStrLn "  4.  Видалити автора"
    putStrLn ""
    putStrLn "СТАТТІ:"
    putStrLn "  5.  Переглянути всі статті"
    putStrLn "  6.  Додати статтю"
    putStrLn "  7.  Редагувати статтю"
    putStrLn "  8.  Видалити статтю"
    putStrLn ""
    putStrLn "КАТЕГОРІЇ:"
    putStrLn "  9.  Переглянути категорії"
    putStrLn "  10. Додати категорію"
    putStrLn ""
    putStrLn "ТЕГИ:"
    putStrLn "  11. Переглянути теги"
    putStrLn "  12. Додати тег"
    putStrLn ""
    putStrLn "ГРАФІЧНІ МАТЕРІАЛИ:"
    putStrLn "  13. Переглянути матеріали"
    putStrLn "  14. Додати матеріал"
    putStrLn "  15. Зв'язати матеріал зі статтею"
    putStrLn ""
    putStrLn "КОМЕНТАРІ:"
    putStrLn "  16. Додати коментар"
    putStrLn "  17. Переглянути коментарі до статті"
    putStrLn ""
    putStrLn "СТАТИСТИКА:"
    putStrLn "  18. Загальна статистика"
    putStrLn "  19. Статистика по категоріях"
    putStrLn "  20. Симулювати перегляд статті"
    putStrLn ""
    putStrLn "  0.  Вихід"
    printSeparator
    
    choice <- prompt "Ваш вибір: "
    
    case choice of
        -- Автори
        "1" -> do
            authors <- Authors.getAllAuthors conn
            Authors.displayAuthors authors
            _ <- prompt "\nНатисніть Enter для продовження..."
            mainMenu conn
        "2" -> do
            Authors.addAuthor conn
            _ <- prompt "\nНатисніть Enter для продовження..."
            mainMenu conn
        "3" -> do
            Authors.updateAuthor conn
            _ <- prompt "\nНатисніть Enter для продовження..."
            mainMenu conn
        "4" -> do
            Authors.deleteAuthor conn
            _ <- prompt "\nНатисніть Enter для продовження..."
            mainMenu conn
        
        -- Статті
        "5" -> do
            articles <- Articles.getAllArticles conn
            Articles.displayArticles conn articles
            _ <- prompt "\nНатисніть Enter для продовження..."
            mainMenu conn
        "6" -> do
            Articles.addArticle conn
            _ <- prompt "\nНатисніть Enter для продовження..."
            mainMenu conn
        "7" -> do
            Articles.updateArticle conn
            _ <- prompt "\nНатисніть Enter для продовження..."
            mainMenu conn
        "8" -> do
            Articles.deleteArticle conn
            _ <- prompt "\nНатисніть Enter для продовження..."
            mainMenu conn
        
        -- Категорії
        "9" -> do
            categories <- Categories.getAllCategories conn
            Categories.displayCategories categories
            _ <- prompt "\nНатисніть Enter для продовження..."
            mainMenu conn
        "10" -> do
            Categories.addCategory conn
            _ <- prompt "\nНатисніть Enter для продовження..."
            mainMenu conn
        
        -- Теги
        "11" -> do
            tags <- Tags.getAllTags conn
            Tags.displayTags tags
            _ <- prompt "\nНатисніть Enter для продовження..."
            mainMenu conn
        "12" -> do
            Tags.addTag conn
            _ <- prompt "\nНатисніть Enter для продовження..."
            mainMenu conn
        
        -- Графічні матеріали
        "13" -> do
            materials <- Materials.getAllMaterials conn
            Materials.displayMaterials conn materials
            _ <- prompt "\nНатисніть Enter для продовження..."
            mainMenu conn
        "14" -> do
            Materials.addMaterial conn
            _ <- prompt "\nНатисніть Enter для продовження..."
            mainMenu conn
        "15" -> do
            materials <- Materials.getAllMaterials conn
            Materials.displayMaterials conn materials
            when (not $ null materials) $ do
                materialIdStr <- prompt "\nВведіть ID матеріалу для зв'язування: "
                case readMaybe materialIdStr :: Maybe Int64 of
                    Just mid -> Materials.linkMaterialToArticle conn mid
                    Nothing -> putStrLn "Невірний ID"
            _ <- prompt "\nНатисніть Enter для продовження..."
            mainMenu conn
        
        -- Коментарі
        "16" -> do
            Comments.addComment conn
            _ <- prompt "\nНатисніть Enter для продовження..."
            mainMenu conn
        "17" -> do
            Comments.viewComments conn
            _ <- prompt "\nНатисніть Enter для продовження..."
            mainMenu conn
        
        -- Статистика
        "18" -> do
            Statistics.viewStatistics conn
            _ <- prompt "\nНатисніть Enter для продовження..."
            mainMenu conn
        "19" -> do
            Statistics.viewCategoryStatistics conn
            _ <- prompt "\nНатисніть Enter для продовження..."
            mainMenu conn
        "20" -> do
            Statistics.simulateView conn
            _ <- prompt "\nНатисніть Enter для продовження..."
            mainMenu conn
        
        -- Вихід
        "0" -> do
            putStrLn "\nДо побачення!"
            return ()
        
        -- Невірний вибір
        _ -> do
            putStrLn "\nНевірний вибір! Спробуйте ще раз."
            _ <- prompt "Натисніть Enter..."
            mainMenu conn
