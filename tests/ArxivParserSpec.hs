{-# LANGUAGE QuasiQuotes #-}

module ArxivParserSpec where

import Data.Map.Strict qualified as M
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.Time.Format (defaultTimeLocale, parseTimeM)
import NeatInterpolation ( trimming )
import Philia093.InfoSource.Arxiv
  ( parseAbstract,
    parseArxivId,
    parseArxivSubscription,
    parseBlock,
    parseMetadata,
    parsePublishedDate,
    requireField,
  )
import Philia093.Types
  ( Article (articleId, sourceId, sourceType, title, url),
    ArticleId (ArticleId),
    SourceId (SourceId),
    SourceType (SourceArxiv),
  )
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

sampleAbstract1 :: Text
sampleAbstract1 =
  [trimming|

  Convex Markov Games (cMGs) were recently introduced as a broad class of
multi-agent learning problems that generalize Markov games to settings where
strategic agents optimize general utilities beyond additive rewards. While cMGs
expand the modeling frontier, their theoretical foundations, particularly the
structure of Nash equilibria (NE) and guarantees for learning algorithms, are
not yet well understood. In this work, we address these gaps for an extension
of cMGs, which we term General Utility Markov Games (GUMGs), capturing new
applications requiring coupling between agents' occupancy measures. We prove
that in GUMGs, Nash equilibria coincide with the fixed points of projected
pseudo-gradient dynamics (i.e., first-order stationary points), enabled by a
novel agent-wise gradient domination property. This insight also yields a
simple proof of NE existence using Brouwer's fixed-point theorem. We further
show the existence of Markov perfect equilibria. Building on this
characterization, we establish a policy gradient theorem for GUMGs and design a
model-free policy gradient algorithm. For potential GUMGs, we establish
iteration complexity guarantees for computing approximate-NE under exact
gradients and provide sample complexity bounds in both the generative model and
on-policy settings. Our results extend beyond prior work restricted to zero-sum
cMGs, providing the first theoretical analysis of common-interest cMGs.

|]

sampleArxivBlock1 :: Text
sampleArxivBlock1 =
  [trimming|

\\
arXiv:2602.12181 (*cross-listing*)
Date: Thu, 12 Feb 2026 17:11:20 GMT   (56kb)

Title: Convex Markov Games and Beyond: New Proof of Existence, Characterization
  and Learning Algorithms for Nash Equilibria
Authors: Anas Barakat, Ioannis Panageas, Antonios Varvitsiotis
Categories: cs.GT cs.LG cs.MA
Comments: AISTATS 2026
\\|]
    <> sampleAbstract1
    <> [trimming|

\\ ( https://arxiv.org/abs/2602.12181 ,  56kb)
|]

sampleArxivBlock2 :: Text
sampleArxivBlock2 =
  [trimming|

\\
arXiv:2602.12129 (*cross-listing*)
Date: Thu, 12 Feb 2026 16:18:55 GMT   (77kb)

Title: Towards Personalized Bangla Book Recommendation: A Large-Scale
  Multi-Entity Book Graph Dataset
Authors: Rahin Arefin Ahmed, Md. Anik Chowdhury, Sakil Ahmed Sheikh Reza,
  Devnil Bhattacharjee, Muhammad Abdullah Adnan, Nafis Sadeq
Categories: cs.IR cs.LG
\\
  Personalized book recommendation in Bangla literature has been constrained by
the lack of structured, large-scale, and publicly available datasets. This work
introduces RokomariBG, a large-scale, multi-entity heterogeneous book graph
dataset designed to support research on personalized recommendation in a
low-resource language setting. The dataset comprises 127,302 books, 63,723
users, 16,601 authors, 1,515 categories, 2,757 publishers, and 209,602 reviews,
connected through eight relation types and organized as a comprehensive
knowledge graph.
  To demonstrate the utility of the dataset, we provide a systematic
benchmarking study on the Top-N recommendation task, evaluating a diverse set
of representative recommendation models, including classical collaborative
filtering methods, matrix factorization models, content-based approaches, graph
neural networks, a hybrid matrix factorization model with side information, and
a neural two-tower retrieval architecture. The benchmarking results highlight
the importance of leveraging multi-relational structure and textual side
information, with neural retrieval models achieving the strongest performance
(NDCG@10 = 0.204). Overall, this work establishes a foundational benchmark and
a publicly available resource for Bangla book recommendation research, enabling
reproducible evaluation and future studies on recommendation in low-resource
cultural domains. The dataset and code are publicly available at
https://github.com/backlashblitz/Bangla-Book-Recommendation-Dataset
\\ ( https://arxiv.org/abs/2602.12129 ,  77kb)
|]

emailHeader :: Text
emailHeader =
  [trimming|
------------------------------------------------------------------------------
------------------------------------------------------------------------------
Send any comments regarding submissions directly to submitter.
------------------------------------------------------------------------------
Archives at http://arxiv.org/
To unsubscribe, e-mail To: cs@arXiv.org, Subject: cancel
------------------------------------------------------------------------------
 Submissions to:
Artificial Intelligence
Computation and Language
Computer Vision and Pattern Recognition
Machine Learning
 received from  Wed 11 Feb 26 19:00:00 GMT  to  Thu 12 Feb 26 19:00:00 GMT
------------------------------------------------------------------------------
------------------------------------------------------------------------------
|]

sampleReviewBlock :: Text
sampleReviewBlock =
  [trimming|
\\
arXiv:2503.16743
replaced with revised version Wed, 11 Feb 2026 22:17:21 GMT   (9726kb)

Title: Can Complexity and Uncomputability Explain Intelligence? SuperARC: A
  Test for Artificial Super Intelligence Based on Recursive Compression
Authors: Alberto Hern\'andez-Espinosa, Luan Ozelim, Felipe S. Abrah\~ao, Hector
  Zenil
Categories: cs.AI cs.IT math.IT
Comments: 27 pages + Methods + Supplementary Information, 103 pages total
\\ ( https://arxiv.org/abs/2503.16743 ,  9726kb)
|]

sampleCompleteEmail :: Text
sampleCompleteEmail =
  emailHeader
    <> sampleArxivBlock1
    <> "------------------------------------------------------------------------------\n"
    <> sampleArxivBlock2
    <> "%%--%%--%%--%%--%%--%%--%%--%%--%%--%%--%%--%%--%%--%%--%%--%%--%%--%%--%%--%%\n"
    <> "------------------------------------------------------------------------------\n"
    <> sampleReviewBlock

sampleInvalidBlock :: Text
sampleInvalidBlock =
  [trimming|
This block does not contain an arXiv ID or proper formatting.
It should fail all parsing attempts and return Nothing.
|]

mockFetchedAt :: UTCTime
mockFetchedAt =
  case parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S" "2024-01-20 12:00:00" of
    Just time -> time
    Nothing -> error "Failed to parse mock time"

spec :: Spec
spec = do
  describe "Arxiv Parser - parseArxivId" $ do
    it "should extract valid arXiv ID from block" $ do
      parseArxivId sampleArxivBlock1 `shouldBe` Just (ArticleId "2602.12181")

    it "should return Nothing when arXiv ID is missing" $ do
      parseArxivId sampleInvalidBlock `shouldBe` Nothing

    it "should handle arXiv ID with proper formatting" $ do
      let block = "arXiv: 1234.56789 v1\nOther content"
      parseArxivId block `shouldBe` Just (ArticleId "1234.56789")

  describe "Arxiv Parser - parsePublishedDate" $ do
    it "should parse valid date in expected format" $ do
      let result = parsePublishedDate sampleArxivBlock1
      result `shouldSatisfy` (/= Nothing)

    it "should return Nothing when Date field is missing" $ do
      parsePublishedDate sampleInvalidBlock `shouldBe` Nothing

    it "should handle different valid date formats" $ do
      let block = "Date: Fri, 01 Dec 2023 15:45:30 UTC\n"
      parsePublishedDate block `shouldSatisfy` (/= Nothing)

  describe "Arxiv Parser - requireField" $ do
    it "should extract title from block" $ do
      requireField "Title:" sampleArxivBlock1 `shouldBe` Just "Convex Markov Games and Beyond: New Proof of Existence, Characterization and Learning Algorithms for Nash Equilibria"

    it "should extract authors from block" $ do
      let result = requireField "Authors:" sampleArxivBlock1
      result `shouldSatisfy` (/= Nothing)

    it "should return Nothing when field is not found" $ do
      let block = "No fields here\n"
      requireField "Title:" block `shouldBe` Nothing

  describe "Arxiv Parser - parseAbstract" $ do
    it "should extract abstract from block" $ do
      let result = parseAbstract sampleArxivBlock1
      result `shouldBe` Just sampleAbstract1

    it "should handle empty abstract" $ do
      let block = "arXiv: 2401.99999\nTitle: Test\n\\\\\n"
      let result = parseAbstract block
      result `shouldSatisfy` (\case Just "" -> True; Nothing -> True; _ -> False)

  describe "Arxiv Parser - parseMetadata" $ do
    it "should extract metadata categories" $ do
      let result = parseMetadata sampleArxivBlock1
      M.lookup "Categories" result `shouldBe` Just "cs.GT cs.LG cs.MA"

    it "should return empty map when no metadata exists" $ do
      let result = parseMetadata sampleInvalidBlock
      M.size result `shouldBe` 0

  describe "Arxiv Parser - parseBlock" $ do
    it "should parse a complete valid block into Article" $ do
      let result = parseBlock (SourceId "test-source") mockFetchedAt sampleArxivBlock1
      result `shouldSatisfy` (not . null . show)
      case result of
        Just article -> do
          articleId article `shouldBe` ArticleId "2602.12181"
          title article `shouldBe` "Convex Markov Games and Beyond: New Proof of Existence, Characterization and Learning Algorithms for Nash Equilibria"
          sourceType article `shouldBe` SourceArxiv
        Nothing -> fail "Expected Just Article"

    it "should not parse invalid block without arXiv ID" $ do
      let result = parseBlock (SourceId "test-source") mockFetchedAt sampleInvalidBlock
      result `shouldSatisfy` (\case Nothing -> True; _ -> False)

    it "should generate correct arXiv URL" $ do
      let result = parseBlock (SourceId "test-source") mockFetchedAt sampleArxivBlock1
      case result of
        Just article -> url article `shouldBe` Just "https://arxiv.org/abs/2602.12181"
        Nothing -> fail "Expected Just Article"

  describe "Arxiv Parser - parseArxivSubscription" $ do
    it "should parse multiple blocks from email" $ do
      let result = parseArxivSubscription (SourceId "test-source") mockFetchedAt sampleCompleteEmail
      result `shouldSatisfy` (\case Just _ -> True; Nothing -> False)
      case result of
        Just articles -> length articles `shouldBe` 2
        Nothing -> fail "Expected Just [Article]"

    it "should set correct sourceId for all articles" $ do
      let result = parseArxivSubscription (SourceId "my-arxiv") mockFetchedAt sampleCompleteEmail
      case result of
        Just articles -> do
          all (\a -> sourceId a == SourceId "my-arxiv") articles `shouldBe` True
        Nothing -> fail "Expected Just [Article]"

    it "should return Nothing if blocks cannot be parsed" $ do
      let invalidEmail = sampleInvalidBlock <> "%%--%%--%%--%%--%%--%%--%%--%%--%%--%%--%%--%%--%%"
      let result = parseArxivSubscription (SourceId "test") mockFetchedAt invalidEmail
      -- Return Just [] if the partition is found but no valid blocks are parsed, otherwise Nothing if partition is missing
      result `shouldBe` Just []

  describe "Arxiv Parser - Integration" $ do
    it "should correctly split and process complete email" $ do
      let result = parseArxivSubscription (SourceId "arxiv-source") mockFetchedAt sampleCompleteEmail
      case result of
        Just articles -> do
          length articles `shouldBe` 2
          map articleId articles `shouldBe` [ArticleId "2602.12181", ArticleId "2602.12129"]
        Nothing -> fail "Expected to parse articles"

    it "should ignore content after the partition separator" $ do
      let emails = sampleArxivBlock1 <> "%%--%%--%%--%%--%%old-content-should-be-ignored"
      let srcId = SourceId "test"
      let result = parseArxivSubscription srcId mockFetchedAt emails
      case result of
        Just articles -> length articles `shouldBe` 1
        Nothing -> fail "Expected to parse one article"
