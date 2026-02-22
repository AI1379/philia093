module Philia093.Types
  ( -- | Core data types
    Email (..),
    EmailId (..),
    Attachment (..),
    Article (..),
    ArticleId (..),
    SourceId (..),
    SourceType (..),
    TargetId (..),
    NotifyType (..),
    -- | Process results
    ProcessResult (..),
    -- | Error handling
    AppError (..),
    -- | Configuration
    EmailConfig (..),
    NotifyConfig (..),
    AppConfig (..),
    -- | Environment
    BotEnv (..),
    AppEnv (..),
    appConfigToBotEnv,
  )
where

import Control.Exception (Exception)
import Data.Aeson
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics
import Philia093.Email.EmailTypes
import Philia093.LLM.LLMTypes

-- ============================================================================
-- | Source Identity & Types (新增：统一的信息源表示)
-- ============================================================================

-- | Unique identifier for information sources
newtype SourceId = SourceId {unSourceId :: Text}
  deriving stock (Show, Generic, Eq, Ord)
  deriving newtype (ToJSON, FromJSON)

-- | Type of information source
data SourceType
  = SourceArxiv
  | SourceEmail
  | SourceRSS
  | SourceWebAPI
  | SourceCustom Text
  deriving (Show, Generic, Eq)

instance ToJSON SourceType

instance FromJSON SourceType

-- | Unique identifier for articles/items from any source
newtype ArticleId = ArticleId {unArticleId :: Text}
  deriving stock (Show, Generic, Eq, Ord)
  deriving newtype (ToJSON, FromJSON)

-- | Unified article representation across all information sources
-- This is the core data type that bridges different sources
data Article = Article
  { articleId :: ArticleId,
    title :: Text,
    author :: Maybe Text,
    url :: Maybe Text,
    excerpt :: Text, -- 摘要或正文内容
    sourceId :: SourceId, -- 来自哪个信息源
    sourceType :: SourceType, -- 信息源类型
    publishedAt :: Maybe UTCTime,
    fetchedAt :: UTCTime,
    metadata :: Map Text Text -- 扩展字段（JSON编码）
  }
  deriving (Show, Generic, Eq)

instance ToJSON Article

instance FromJSON Article

-- ============================================================================
-- | Notification Identity & Types (新增：统一的通知目标表示)
-- ============================================================================

-- | Unique identifier for notification targets
newtype TargetId = TargetId {unTargetId :: Text}
  deriving stock (Show, Generic, Eq, Ord)
  deriving newtype (ToJSON, FromJSON)

-- | Type of notification method
data NotifyType
  = NotifyWebhook
  | NotifyEmail
  | NotifyMatrix
  | NotifyTelegram
  | NotifyCustom Text
  deriving (Show, Generic, Eq)

instance ToJSON NotifyType

instance FromJSON NotifyType

-- ============================================================================
-- | Processing Results (改进：关联信息源)
-- ============================================================================

-- | Result of processing an article from any source
data ProcessResult = ProcessResult
  { article :: Article, -- 关联原始文章
    handled :: Bool,
    summary :: Text, -- LLM 总结
    relevanceScore :: Float, -- 相关性评分 (0-1)
    shouldNotify :: Bool,
    error :: Maybe AppError,
    processedAt :: UTCTime
  }
  deriving (Show, Generic)


instance ToJSON ProcessResult

instance FromJSON ProcessResult

-- ============================================================================
-- | Error Handling
-- ============================================================================

-- | Application errors - proper sum type for different error cases
data AppError
  = EmailFetchError Text
  | EmailProcessError Text
  | NotificationError Text
  | LLMError' LLMError
  | ConfigError Text
  | InfoSourceError SourceId Text -- 新增：信息源相关的错误
  | ProcessingError Text
  deriving (Show, Generic)

instance Exception AppError

instance ToJSON AppError

instance FromJSON AppError

-- ============================================================================
-- | Configuration
-- ============================================================================

-- | Notification configuration
newtype NotifyConfig = NotifyConfig
  { webhookUrl :: Text
  }
  deriving stock (Show, Generic)
  deriving newtype (ToJSON, FromJSON)

-- | Complete application configuration
data AppConfig = AppConfig
  { emailConfig :: EmailConfig,
    notifyConfig :: NotifyConfig,
    llmConfig :: LLMConfig
  }
  deriving (Show, Generic)

instance ToJSON AppConfig where
  toJSON AppConfig {..} =
    object
      [ "email" .= emailConfig,
        "notify" .= notifyConfig,
        "llm" .= llmConfig
      ]

instance FromJSON AppConfig where
  parseJSON = withObject "AppConfig" $ \v ->
    AppConfig
      <$> v .: "email"
      <*> v .: "notify"
      <*> v .: "llm"

-- ============================================================================
-- | Runtime Environment
-- ============================================================================

-- | Bot environment using the ReaderT pattern (保留原有设计)
-- This replaces the deep transformer stack with a single unified environment
data BotEnv = BotEnv
  { botEmailConfig :: EmailConfig,
    botNotifyConfig :: NotifyConfig,
    botLLMConfig :: LLMConfig
  }
  deriving (Show, Generic)

-- | Extended application environment (新增：支持多源多通知)
-- 目前暂不使用，为未来扩展预留
data AppEnv = AppEnv
  { appConfig :: AppConfig
    -- 可以添加：
    -- , infoSources :: Map SourceId (SomeInfoSource m)
    -- , notifiers :: Map TargetId (SomeNotifier m)
    -- , cache :: IORef ProcessingCache
  }
  deriving (Show, Generic)

-- | Helper to create BotEnv from AppConfig
appConfigToBotEnv :: AppConfig -> BotEnv
appConfigToBotEnv (AppConfig email notify llm) =
  BotEnv
    { botEmailConfig = email,
      botNotifyConfig = notify,
      botLLMConfig = llm
    }
