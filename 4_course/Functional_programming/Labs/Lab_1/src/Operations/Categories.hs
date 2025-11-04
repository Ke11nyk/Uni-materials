{-# LANGUAGE OverloadedStrings #-}

module Operations.Categories
    ( getAllCategories
    , displayCategories
    , addCategory
    , getCategoryName
    ) where

import Database.MySQL.Simple
import qualified Data.Text as T
import Data.Int
import Control.Monad (forM_, void)

import Models.Types
import UI.Utils

-- | Отримати всі категорії
getAllCategories :: Connection -> IO [Category]
getAllCategories conn = do
    results <- query_ conn 
        "SELECT category_id, category_name, description FROM categories ORDER BY category_name" 
        :: IO [(Int64, T.Text, Maybe T.Text)]
    return [Category cid cname cdesc | (cid, cname, cdesc) <- results]

-- | Відобразити список категорій
displayCategories :: [Category] -> IO ()
displayCategories categories = do
    printSeparator
    putStrLn "СПИСОК КАТЕГОРІЙ"
    printSeparator
    if null categories
        then putStrLn "Немає категорій."
        else forM_ categories $ \c -> do
            putStrLn $ show (categoryId c) ++ ". " ++ T.unpack (categoryName c)
            case categoryDescription c of
                Just desc -> putStrLn $ "   " ++ T.unpack desc
                Nothing -> return ()
            putStrLn ""

-- | Додати нову категорію
addCategory :: Connection -> IO ()
addCategory conn = do
    clearScreen
    printSeparator
    putStrLn "ДОДАВАННЯ НОВОЇ КАТЕГОРІЇ"
    printSeparator
    
    name <- promptText "Назва категорії: "
    desc <- promptText "Опис (Enter - пропустити): "
    
    let descValue = if T.null desc then Nothing else Just desc
    
    void $ execute conn 
        "INSERT INTO categories (category_name, description) VALUES (?, ?)"
        (name, descValue)
    
    putStrLn "\n✓ Категорію успішно додано!"

-- | Отримати назву категорії за ID
getCategoryName :: Connection -> Int64 -> IO T.Text
getCategoryName conn catId = do
    results <- query conn 
        "SELECT category_name FROM categories WHERE category_id = ?" 
        (Only catId) :: IO [Only T.Text]
    case results of
        [Only name] -> return name
        _ -> return "Невідома категорія"