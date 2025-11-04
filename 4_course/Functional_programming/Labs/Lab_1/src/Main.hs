{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import System.IO (hSetEncoding, stdin, stdout, utf8)
import Database.MySQL.Simple (close)
import Configuration.Dotenv (loadFile, defaultConfig)
import Control.Exception (catch, SomeException)
import System.Exit (exitFailure)

import Database.Config (loadDbConfig, initConnection)
import UI.Menu (mainMenu)

-- =====================================================
-- ГОЛОВНА ФУНКЦІЯ
-- =====================================================

main :: IO ()
main = do
    -- Налаштування UTF-8 кодування для введення/виведення
    hSetEncoding stdin utf8
    hSetEncoding stdout utf8
    
    putStrLn "==================================================="
    putStrLn "  СИСТЕМА 'МЕРЕЖЕВА ГАЗЕТА ФАКУЛЬТЕТУ'"
    putStrLn "==================================================="
    putStrLn ""
    
    -- Завантаження .env файлу (якщо існує)
    putStrLn "Завантаження конфігурації..."
    envLoaded <- catch 
        (loadFile defaultConfig >> return True)
        (\(_ :: SomeException) -> return False)
    
    if envLoaded
        then putStrLn "✓ Файл .env завантажено"
        else putStrLn "⚠ Файл .env не знайдено, використовуються системні змінні оточення"
    
    putStrLn ""
    
    -- Підключення до БД
    -- Якщо змінні відсутні - loadDbConfig завершить програму з помилкою
    putStrLn "Перевірка налаштувань бази даних..."
    config <- loadDbConfig
    
    putStrLn "Підключення до бази даних..."
    conn <- catch 
        (initConnection config)
        (\(e :: SomeException) -> do
            putStrLn $ "✗ Помилка підключення до бази даних: " ++ show e
            putStrLn ""
            putStrLn "Перевірте:"
            putStrLn "  • Чи запущений MySQL?"
            putStrLn "  • Чи правильні дані підключення в .env?"
            putStrLn "  • Чи існує база даних faculty_newspaper?"
            putStrLn ""
            putStrLn "Для ініціалізації БД виконайте:"
            putStrLn "  mysql -u root -p < database_init.sql"
            exitFailure
        )
    
    putStrLn "✓ Підключено успішно!"
    putStrLn ""
    putStrLn "Нові можливості:"
    putStrLn "  • Використання category_id замість текстових категорій"
    putStrLn "  • Робота з тегами статей"
    putStrLn "  • Графічні матеріали з типами"
    putStrLn "  • Розширена статистика"
    putStrLn "  • Конфігурація через .env файл"
    putStrLn ""
    
    -- Запуск головного меню
    mainMenu conn
    
    -- Закриття з'єднання
    close conn
    putStrLn ""
    putStrLn "==================================================="
    putStrLn "  Систему завершено. Дякуємо за використання!"
    putStrLn "==================================================="
