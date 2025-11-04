{-# LANGUAGE OverloadedStrings #-}

module UI.Utils
    ( safeGetLine
    , prompt
    , promptText
    , clearScreen
    , printSeparator
    ) where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as BS
import System.IO (hFlush, stdout)
import Control.Exception (catch, SomeException)

-- | Безпечне читання UTF-8 тексту
safeGetLine :: IO T.Text
safeGetLine = catch TIO.getLine handleError
  where
    handleError :: SomeException -> IO T.Text
    handleError _ = do
        -- Якщо TIO.getLine не працює, читаємо як ByteString і декодуємо
        bs <- BS.getLine
        return $ TE.decodeUtf8 bs

-- | Вивести запит і отримати відповідь у вигляді String
prompt :: String -> IO String
prompt text = do
    putStr text
    hFlush stdout
    T.unpack <$> safeGetLine

-- | Вивести запит і отримати відповідь у вигляді Text
promptText :: String -> IO T.Text
promptText text = do
    putStr text
    hFlush stdout
    safeGetLine

-- | Очистити екран (простий варіант)
clearScreen :: IO ()
clearScreen = putStrLn "\n\n"

-- | Вивести роздільник
printSeparator :: IO ()
printSeparator = putStrLn "=================================================="