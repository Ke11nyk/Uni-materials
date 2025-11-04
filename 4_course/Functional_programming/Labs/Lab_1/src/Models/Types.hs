{-# LANGUAGE OverloadedStrings #-}

module Models.Types where

import qualified Data.Text as T
import Data.Int
import Data.Time

-- =====================================================
-- ОСНОВНІ ТИПИ ДАНИХ
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

data Category = Category
    { categoryId :: Int64
    , categoryName :: T.Text
    , categoryDescription :: Maybe T.Text
    } deriving (Show)

data Tag = Tag
    { tagId :: Int64
    , tagName :: T.Text
    , tagUsageCount :: Int64
    } deriving (Show)

data Article = Article
    { articleId :: Int64
    , articleTitle :: T.Text
    , articleAuthorId :: Int64
    , articleAnnotation :: T.Text
    , articleFilePath :: T.Text
    , articleCategoryId :: Int64
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

data GraphicMaterial = GraphicMaterial
    { materialId :: Int64
    , materialTitle :: T.Text
    , materialAuthorId :: Int64
    , materialFilePath :: T.Text
    , materialType :: MaterialType
    , materialDescription :: Maybe T.Text
    , materialArticleId :: Maybe Int64  -- Зв'язок зі статтею
    } deriving (Show)

data MaterialType 
    = Image
    | Infographic
    | Chart
    | Diagram
    deriving (Show, Eq)

-- Конвертація MaterialType в Text для бази даних
materialTypeToText :: MaterialType -> T.Text
materialTypeToText Image = "image"
materialTypeToText Infographic = "infographic"
materialTypeToText Chart = "chart"
materialTypeToText Diagram = "diagram"

-- Конвертація Text в MaterialType
textToMaterialType :: T.Text -> Maybe MaterialType
textToMaterialType "image" = Just Image
textToMaterialType "infographic" = Just Infographic
textToMaterialType "chart" = Just Chart
textToMaterialType "diagram" = Just Diagram
textToMaterialType _ = Nothing