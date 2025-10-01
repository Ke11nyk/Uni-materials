{-# LANGUAGE OverloadedStrings #-}

module Main where

import Database.MySQL.Simple
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import Data.Time
import Data.Int
import Control.Monad (forM_, when, void)
import System.IO (hFlush, stdout, stdin, hSetEncoding, utf8)
import Text.Read (readMaybe)
import qualified Data.ByteString as BS
import Control.Exception (catch, SomeException)

-- =====================================================
-- ТИПИ ДАНИХ
-- =====================================================

data Author = Author
    { authorId :: Int64
    , authorFirstName :: T.Text
    , authorLastName :: T.Text
    , authorEmail :: T.Text
    , authorDepartment :: T.Text
    } deriving (Show)

data Reader = Reader
    { readerId :: Int64
    , readerFirstName :: T.Text
    , readerLastName :: T.Text
    , readerEmail :: T.Text
    , studentGroup :: Maybe T.Text
    } deriving (Show)

data Article = Article
    { articleId :: Int64
    , articleTitle :: T.Text
    , articleAuthorId :: Int64
    , articleAnnotation :: T.Text
    , articleFilePath :: T.Text
    , articleCategory :: T.Text
    , articleIsPublished :: Bool
    } deriving (Show)

data Comment = Comment
    { commentId :: Int64
    , commentArticleId :: Int64
    , commentReaderId :: Int64
    , commentText :: T.Text
    , commentRating :: Maybe Int
    } deriving (Show)

data ArticleStatistics = ArticleStatistics
    { statArticleId :: Int64
    , statViewCount :: Int64
    , statDownloadCount :: Int64
    , statCommentCount :: Int64
    , statAverageRating :: Double
    } deriving (Show)

-- =====================================================
-- КОНФІГУРАЦІЯ БАЗИ ДАНИХ
-- =====================================================

dbConfig :: ConnectInfo
dbConfig = defaultConnectInfo
    { connectHost = "localhost"
    , connectPort = 3306
    , connectUser = "root"
    , connectPassword = "password"
    , connectDatabase = "faculty_newspaper"
    }

-- =====================================================
-- ДОПОМІЖНІ ФУНКЦІЇ
-- =====================================================

-- Безпечне читання UTF-8 тексту
safeGetLine :: IO T.Text
safeGetLine = catch TIO.getLine handleError
  where
    handleError :: SomeException -> IO T.Text
    handleError _ = do
        -- Якщо TIO.getLine не працює, читаємо як ByteString і декодуємо
        bs <- BS.getLine
        return $ TE.decodeUtf8 bs

prompt :: String -> IO String
prompt text = do
    putStr text
    hFlush stdout
    T.unpack <$> safeGetLine

promptText :: String -> IO T.Text
promptText text = do
    putStr text
    hFlush stdout
    safeGetLine

clearScreen :: IO ()
clearScreen = putStrLn "\n\n"

printSeparator :: IO ()
printSeparator = putStrLn "=================================================="

-- =====================================================
-- ФУНКЦІЇ РОБОТИ З АВТОРАМИ
-- =====================================================

getAllAuthors :: Connection -> IO [Author]
getAllAuthors conn = do
    results <- query_ conn 
        "SELECT author_id, first_name, last_name, email, department FROM authors ORDER BY last_name" 
        :: IO [(Int64, T.Text, T.Text, T.Text, T.Text)]
    return [Author aid fname lname email dept | (aid, fname, lname, email, dept) <- results]

displayAuthors :: [Author] -> IO ()
displayAuthors authors = do
    printSeparator
    putStrLn "СПИСОК АВТОРІВ"
    printSeparator
    if null authors
        then putStrLn "Немає зареєстрованих авторів."
        else forM_ authors $ \a -> do
            putStrLn $ show (authorId a) ++ ". " ++ 
                       T.unpack (authorFirstName a) ++ " " ++ 
                       T.unpack (authorLastName a)
            putStrLn $ "   Email: " ++ T.unpack (authorEmail a)
            putStrLn $ "   Кафедра: " ++ T.unpack (authorDepartment a)
            putStrLn ""

addAuthor :: Connection -> IO ()
addAuthor conn = do
    clearScreen
    printSeparator
    putStrLn "ДОДАВАННЯ НОВОГО АВТОРА"
    printSeparator
    
    fname <- promptText "Ім'я: "
    lname <- promptText "Прізвище: "
    email <- promptText "Email: "
    dept <- promptText "Кафедра: "
    
    void $ execute conn 
        "INSERT INTO authors (first_name, last_name, email, department) VALUES (?, ?, ?, ?)"
        (fname, lname, email, dept)
    
    putStrLn "\n✓ Автора успішно додано!"

updateAuthor :: Connection -> IO ()
updateAuthor conn = do
    clearScreen
    authors <- getAllAuthors conn
    displayAuthors authors
    
    when (not $ null authors) $ do
        authorIdStr <- prompt "\nВведіть ID автора для редагування: "
        case readMaybe authorIdStr :: Maybe Int64 of
            Just aid -> do
                fname <- promptText "Нове ім'я (Enter - пропустити): "
                lname <- promptText "Нове прізвище (Enter - пропустити): "
                dept <- promptText "Нова кафедра (Enter - пропустити): "
                
                when (not $ T.null fname) $
                    void $ execute conn "UPDATE authors SET first_name = ? WHERE author_id = ?" (fname, aid)
                when (not $ T.null lname) $
                    void $ execute conn "UPDATE authors SET last_name = ? WHERE author_id = ?" (lname, aid)
                when (not $ T.null dept) $
                    void $ execute conn "UPDATE authors SET department = ? WHERE author_id = ?" (dept, aid)
                
                putStrLn "\n✓ Дані автора оновлено!"
            Nothing -> putStrLn "\n✗ Невірний ID!"

deleteAuthor :: Connection -> IO ()
deleteAuthor conn = do
    clearScreen
    authors <- getAllAuthors conn
    displayAuthors authors
    
    when (not $ null authors) $ do
        authorIdStr <- prompt "\nВведіть ID автора для видалення: "
        case readMaybe authorIdStr :: Maybe Int64 of
            Just aid -> do
                void $ execute conn "DELETE FROM authors WHERE author_id = ?" (Only aid)
                putStrLn "\n✓ Автора видалено!"
            Nothing -> putStrLn "\n✗ Невірний ID!"

-- =====================================================
-- ФУНКЦІЇ РОБОТИ ЗІ СТАТТЯМИ
-- =====================================================

getAllArticles :: Connection -> IO [Article]
getAllArticles conn = do
    results <- query_ conn 
        "SELECT article_id, title, author_id, annotation, file_path, category, is_published FROM articles ORDER BY publication_date DESC"
        :: IO [(Int64, T.Text, Int64, T.Text, T.Text, T.Text, Bool)]
    return [Article aid title authId annot path cat pub | (aid, title, authId, annot, path, cat, pub) <- results]

displayArticles :: Connection -> [Article] -> IO ()
displayArticles conn articles = do
    printSeparator
    putStrLn "СПИСОК СТАТЕЙ"
    printSeparator
    if null articles
        then putStrLn "Немає опублікованих статей."
        else forM_ articles $ \a -> do
            -- Отримуємо ім'я автора
            authorResults <- query conn 
                "SELECT first_name, last_name FROM authors WHERE author_id = ?" 
                (Only $ articleAuthorId a) :: IO [(T.Text, T.Text)]
            
            let authorName = case authorResults of
                    [(fname, lname)] -> T.unpack fname ++ " " ++ T.unpack lname
                    _ -> "Невідомий автор"
            
            putStrLn $ show (articleId a) ++ ". " ++ T.unpack (articleTitle a)
            putStrLn $ "   Автор: " ++ authorName
            putStrLn $ "   Категорія: " ++ T.unpack (articleCategory a)
            putStrLn $ "   Статус: " ++ if articleIsPublished a then "✓ Опубліковано" else "○ Чернетка"
            putStrLn $ "   Анотація: " ++ take 100 (T.unpack $ articleAnnotation a) ++ "..."
            putStrLn ""

addArticle :: Connection -> IO ()
addArticle conn = do
    clearScreen
    printSeparator
    putStrLn "ДОДАВАННЯ НОВОЇ СТАТТІ"
    printSeparator
    
    authors <- getAllAuthors conn
    if null authors
        then putStrLn "Спочатку додайте хоча б одного автора!"
        else do
            displayAuthors authors
            
            title <- promptText "Назва статті: "
            authorIdStr <- prompt "ID автора: "
            annotation <- promptText "Анотація: "
            filePath <- promptText "Шлях до файлу: "
            category <- promptText "Категорія (Наука/Технології/Освіта/Студентське життя/Факультет): "
            publishedStr <- prompt "Опублікувати одразу? (y/n): "
            
            case readMaybe authorIdStr :: Maybe Int64 of
                Just authId -> do
                    let isPublished = publishedStr == "y" || publishedStr == "Y"
                    void $ execute conn 
                        "INSERT INTO articles (title, author_id, annotation, file_path, category, is_published) VALUES (?, ?, ?, ?, ?, ?)"
                        (title, authId, annotation, filePath, category, isPublished)
                    
                    -- Створюємо запис статистики (отримуємо ID через SELECT LAST_INSERT_ID())
                    [Only lastId] <- query_ conn "SELECT LAST_INSERT_ID()" :: IO [Only Int64]
                    void $ execute conn 
                        "INSERT INTO article_statistics (article_id) VALUES (?)"
                        (Only lastId)
                    
                    putStrLn "\n✓ Статтю успішно додано!"
                Nothing -> putStrLn "\n✗ Невірний ID автора!"

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
                annotation <- promptText "Нова анотація (Enter - пропустити): "
                category <- promptText "Нова категорія (Enter - пропустити): "
                publishedStr <- prompt "Опублікувати? (y/n/Enter - пропустити): "
                
                when (not $ T.null title) $
                    void $ execute conn "UPDATE articles SET title = ? WHERE article_id = ?" (title, aid)
                when (not $ T.null annotation) $
                    void $ execute conn "UPDATE articles SET annotation = ? WHERE article_id = ?" (annotation, aid)
                when (not $ T.null category) $
                    void $ execute conn "UPDATE articles SET category = ? WHERE article_id = ?" (category, aid)
                when (publishedStr == "y" || publishedStr == "Y") $
                    void $ execute conn "UPDATE articles SET is_published = TRUE WHERE article_id = ?" (Only aid)
                when (publishedStr == "n" || publishedStr == "N") $
                    void $ execute conn "UPDATE articles SET is_published = FALSE WHERE article_id = ?" (Only aid)
                
                putStrLn "\n✓ Статтю оновлено!"
            Nothing -> putStrLn "\n✗ Невірний ID!"

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

-- =====================================================
-- ФУНКЦІЇ РОБОТИ З КОМЕНТАРЯМИ
-- =====================================================

addComment :: Connection -> IO ()
addComment conn = do
    clearScreen
    articles <- getAllArticles conn
    displayArticles conn articles
    
    when (not $ null articles) $ do
        articleIdStr <- prompt "\nВведіть ID статті для коментування: "
        
        -- Отримуємо або створюємо читача
        readerEmail <- promptText "Ваш email: "
        readers <- query conn "SELECT reader_id FROM readers WHERE email = ?" (Only readerEmail)
            :: IO [Only Int64]
        
        readerId <- case readers of
            [Only rid] -> return rid
            _ -> do
                fname <- promptText "Ваше ім'я: "
                lname <- promptText "Ваше прізвище: "
                grp <- promptText "Група (Enter - пропустити): "
                
                let grpMaybe = if T.null grp then Nothing else Just grp
                void $ execute conn 
                    "INSERT INTO readers (first_name, last_name, email, student_group) VALUES (?, ?, ?, ?)"
                    (fname, lname, readerEmail, grpMaybe)
                
                -- Отримуємо ID через SELECT LAST_INSERT_ID()
                [Only newId] <- query_ conn "SELECT LAST_INSERT_ID()" :: IO [Only Int64]
                return newId
        
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
                        "UPDATE article_statistics SET average_rating = (SELECT AVG(rating) FROM comments WHERE article_id = ? AND rating IS NOT NULL) WHERE article_id = ?"
                        (aid, aid)
                
                putStrLn "\n✓ Коментар додано!"
            Nothing -> putStrLn "\n✗ Невірний ID статті!"

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
                    "SELECT c.comment_id, c.comment_text, c.rating, r.first_name, r.last_name FROM comments c JOIN readers r ON c.reader_id = r.reader_id WHERE c.article_id = ? ORDER BY c.comment_date DESC"
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

-- =====================================================
-- СТАТИСТИКА
-- =====================================================

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
    
    putStrLn $ "Авторів: " ++ show authorCount
    putStrLn $ "Статей: " ++ show articleCount ++ " (опубліковано: " ++ show publishedCount ++ ")"
    putStrLn $ "Читачів: " ++ show readerCount
    putStrLn $ "Коментарів: " ++ show commentCount
    putStrLn ""
    
    -- Топ-5 статей за переглядами
    printSeparator
    putStrLn "ТОП-5 СТАТЕЙ ЗА ПЕРЕГЛЯДАМИ"
    printSeparator
    topArticles <- query_ conn 
        "SELECT a.title, s.view_count FROM articles a JOIN article_statistics s ON a.article_id = s.article_id ORDER BY s.view_count DESC LIMIT 5"
        :: IO [(T.Text, Int64)]
    
    if null topArticles
        then putStrLn "Немає даних."
        else forM_ topArticles $ \(title, views) ->
            putStrLn $ "• " ++ T.unpack title ++ " — " ++ show views ++ " переглядів"

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

-- =====================================================
-- ГОЛОВНЕ МЕНЮ
-- =====================================================

mainMenu :: Connection -> IO ()
mainMenu conn = do
    clearScreen
    printSeparator
    putStrLn "МЕРЕЖЕВА ГАЗЕТА ФАКУЛЬТЕТУ"
    printSeparator
    putStrLn "1.  Переглянути всіх авторів"
    putStrLn "2.  Додати автора"
    putStrLn "3.  Редагувати автора"
    putStrLn "4.  Видалити автора"
    putStrLn ""
    putStrLn "5.  Переглянути всі статті"
    putStrLn "6.  Додати статтю"
    putStrLn "7.  Редагувати статтю"
    putStrLn "8.  Видалити статтю"
    putStrLn ""
    putStrLn "9.  Додати коментар"
    putStrLn "10. Переглянути коментарі до статті"
    putStrLn ""
    putStrLn "11. Переглянути статистику"
    putStrLn "12. Симулювати перегляд статті"
    putStrLn ""
    putStrLn "0.  Вихід"
    printSeparator
    
    choice <- prompt "Ваш вибір: "
    
    case choice of
        "1" -> do
            authors <- getAllAuthors conn
            displayAuthors authors
            _ <- prompt "\nНатисніть Enter для продовження..."
            mainMenu conn
        "2" -> do
            addAuthor conn
            _ <- prompt "\nНатисніть Enter для продовження..."
            mainMenu conn
        "3" -> do
            updateAuthor conn
            _ <- prompt "\nНатисніть Enter для продовження..."
            mainMenu conn
        "4" -> do
            deleteAuthor conn
            _ <- prompt "\nНатисніть Enter для продовження..."
            mainMenu conn
        "5" -> do
            articles <- getAllArticles conn
            displayArticles conn articles
            _ <- prompt "\nНатисніть Enter для продовження..."
            mainMenu conn
        "6" -> do
            addArticle conn
            _ <- prompt "\nНатисніть Enter для продовження..."
            mainMenu conn
        "7" -> do
            updateArticle conn
            _ <- prompt "\nНатисніть Enter для продовження..."
            mainMenu conn
        "8" -> do
            deleteArticle conn
            _ <- prompt "\nНатисніть Enter для продовження..."
            mainMenu conn
        "9" -> do
            addComment conn
            _ <- prompt "\nНатисніть Enter для продовження..."
            mainMenu conn
        "10" -> do
            viewComments conn
            _ <- prompt "\nНатисніть Enter для продовження..."
            mainMenu conn
        "11" -> do
            viewStatistics conn
            _ <- prompt "\nНатисніть Enter для продовження..."
            mainMenu conn
        "12" -> do
            simulateView conn
            _ <- prompt "\nНатисніть Enter для продовження..."
            mainMenu conn
        "0" -> do
            putStrLn "\nДо побачення!"
            return ()
        _ -> do
            putStrLn "\nНевірний вибір! Спробуйте ще раз."
            _ <- prompt "Натисніть Enter..."
            mainMenu conn

-- =====================================================
-- ГОЛОВНА ФУНКЦІЯ
-- =====================================================

main :: IO ()
main = do
    -- Налаштування UTF-8 кодування для введення/виведення
    hSetEncoding stdin utf8
    hSetEncoding stdout utf8
    
    putStrLn "=== Запуск системи 'Мережева газета факультету' ==="
    putStrLn "\nПідключення до бази даних..."
    
    conn <- connect dbConfig
    
    -- КРИТИЧНО: Встановлюємо UTF-8 для з'єднання з MySQL
    void $ execute_ conn "SET NAMES 'utf8mb4'"
    void $ execute_ conn "SET CHARACTER SET utf8mb4"
    void $ execute_ conn "SET character_set_connection=utf8mb4"
    void $ execute_ conn "SET character_set_results=utf8mb4"
    void $ execute_ conn "SET character_set_client=utf8mb4"
    
    putStrLn "✓ Підключено успішно!\n"
    
    mainMenu conn
    
    close conn
    putStrLn "\n=== Система завершила роботу ==="