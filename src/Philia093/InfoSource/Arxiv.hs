module Philia093.InfoSource.Arxiv where

import Control.Monad ((>=>))
import Data.Functor ((<&>))
import Data.List (find)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Text (Text, unpack)
import Data.Text qualified as T
import Data.Time.Clock (UTCTime)
import Data.Time.Format (defaultTimeLocale, parseTimeM)
import Philia093.InfoSource
  ( Article (..),
    ArticleId (..),
    SourceId,
    SourceType (SourceArxiv),
  )
import Philia093.Utilities ((??))

arxivEmailPartSplitter :: Text
arxivEmailPartSplitter = "%%--%%--%%--%%--%%--%%--%%--%%--%%--%%--%%--%%--%%--%%--%%--%%--%%--%%--%%--%%"

arxivEmailSectionSplitter :: Text
arxivEmailSectionSplitter = "------------------------------------------------------------------------------"

takeNewPart :: Text -> Text
takeNewPart = fst . T.breakOn arxivEmailPartSplitter

splitBlock :: Text -> [Text]
splitBlock = T.splitOn arxivEmailSectionSplitter

findLine :: Text -> Text -> Maybe Text
findLine prefix = find (T.isPrefixOf prefix) . T.lines

findLineWithIndex :: Text -> Text -> Maybe (Int, Text)
findLineWithIndex prefix text =
  listToMaybe $
    mapMaybe
      (\(i, line) -> if T.isPrefixOf prefix line then Just (i, line) else Nothing)
      (zip [0 ..] (T.lines text))

generateArxivUrl :: ArticleId -> Text
generateArxivUrl arxivId = "https://arxiv.org/abs/" <> unArticleId arxivId

parseArxivId :: Text -> Maybe ArticleId
parseArxivId =
  findLine "arXiv:" >=> \line ->
    case T.stripPrefix "arXiv:" (T.strip line) of
      Nothing -> Nothing
      Just rest ->
        let idPart = T.takeWhile (/= ' ') (T.strip rest)
         in if T.null idPart
              then Nothing
              else Just (ArticleId idPart)

parsePublishedDate :: Text -> Maybe UTCTime
parsePublishedDate =
  findLine "Date:" >=> \line ->
    let dateStr = T.strip $ T.stripPrefix "Date:" line ?? ""
        removeTail = T.strip . fst . T.breakOn "("
     in parseTimeM True defaultTimeLocale "%a, %d %b %Y %H:%M:%S %Z" (unpack $ removeTail dateStr)

requireField :: Text -> Text -> Maybe Text
requireField prefix text = do
  (lineIndex, line) <- findLineWithIndex prefix text
  let value = T.strip $ T.stripPrefix prefix line ?? ""
  let rest = T.unwords $ mapMaybe (T.stripPrefix "  ") $ takeWhile ("  " `T.isPrefixOf`) (drop (lineIndex + 1) $ T.lines text)
  pure $ T.strip $ value <> " " <> rest

parseAbstract :: Text -> Maybe Text
parseAbstract block =
  listToMaybe $
    drop
      2
      (T.splitOn "\\\\" block <&> (T.strip . fst . T.breakOn "\\\\"))

parseMetadata :: Text -> Map Text Text
parseMetadata block =
  let keys = ["Categories", "Comments", "MSC-class"]
      extract k = findLine (k <> ":") block >>= \l -> Just (k, T.strip $ T.drop (T.length k + 1) l)
   in M.fromList $ mapMaybe extract keys

parseBlock :: SourceId -> UTCTime -> Text -> Maybe Article
parseBlock sourceId fetchedAt block =
  parseArxivId block >>= \aid ->
    pure
      Article
        { articleId = aid,
          title = requireField "Title:" block ?? "No Title",
          author = requireField "Authors:" block,
          url = Just $ generateArxivUrl aid,
          excerpt = requireField "Abstract:" block ?? "No Abstract",
          sourceId = sourceId,
          sourceType = SourceArxiv,
          publishedAt = parsePublishedDate block,
          fetchedAt = fetchedAt,
          metadata = parseMetadata block
        }

parseArxivSubscription :: SourceId -> UTCTime -> Text -> Maybe [Article]
parseArxivSubscription srcId fetchedAt rawText =
  case takeNewPart rawText of
    "" -> Nothing
    newSection ->
      Just $ mapMaybe (parseBlock srcId fetchedAt) (splitBlock newSection)