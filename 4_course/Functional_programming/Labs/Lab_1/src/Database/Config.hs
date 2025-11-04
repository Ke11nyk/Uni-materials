{-# LANGUAGE OverloadedStrings #-}

module Database.Config
    ( loadDbConfig
    , initConnection
    ) where

import Database.MySQL.Simple
import Control.Monad (void)
import System.Environment (lookupEnv)
import System.Exit (exitFailure)
import Text.Read (readMaybe)

-- | Завантажити конфігурацію з змінних оточення
-- Виходить з програми якщо не всі змінні встановлені
loadDbConfig :: IO ConnectInfo
loadDbConfig = do
    host <- getEnvRequired "DB_HOST"
    portStr <- getEnvRequired "DB_PORT"
    user <- getEnvRequired "DB_USER"
    password <- getEnvRequired "DB_PASSWORD"
    database <- getEnvRequired "DB_NAME"
    
    port <- case readMaybe portStr of
                Just p -> return (p :: Int)
                Nothing -> do
                    putStrLn $ "✗ Помилка: DB_PORT має бути числом, отримано: " ++ portStr
                    exitFailure
    
    return defaultConnectInfo
        { connectHost = host
        , connectPort = fromIntegral port
        , connectUser = user
        , connectPassword = password
        , connectDatabase = database
        }

-- | Отримати обов'язкову змінну оточення
-- Якщо відсутня - видає помилку і завершує програму
getEnvRequired :: String -> IO String
getEnvRequired key = do
    maybeValue <- lookupEnv key
    case maybeValue of
        Just value | not (null value) -> return value
        _ -> do
            putStrLn $ "✗ Помилка: Відсутня обов'язкова змінна оточення: " ++ key
            putStrLn ""
            putStrLn "Створіть файл .env з наступними змінними:"
            putStrLn "  DB_HOST=localhost"
            putStrLn "  DB_PORT=3306"
            putStrLn "  DB_USER=root"
            putStrLn "  DB_PASSWORD=your_password"
            putStrLn "  DB_NAME=faculty_newspaper"
            putStrLn ""
            putStrLn "Або експортуйте змінні:"
            putStrLn "  export DB_HOST=localhost"
            putStrLn "  export DB_PORT=3306"
            putStrLn "  ..."
            exitFailure

-- | Ініціалізація з'єднання з налаштуванням UTF-8
initConnection :: ConnectInfo -> IO Connection
initConnection config = do
    conn <- connect config
    
    -- Встановлюємо UTF-8 для з'єднання з MySQL
    void $ execute_ conn "SET NAMES 'utf8mb4'"
    void $ execute_ conn "SET CHARACTER SET utf8mb4"
    void $ execute_ conn "SET character_set_connection=utf8mb4"
    void $ execute_ conn "SET character_set_results=utf8mb4"
    void $ execute_ conn "SET character_set_client=utf8mb4"
    
    return conn