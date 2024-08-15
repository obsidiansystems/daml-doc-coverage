module Language.DAML.Documentation where

import Data.Aeson
import Data.Data
import Data.Foldable
import Data.Text (Text)
import Data.Text qualified as T
import Lens.Micro hiding (failing)
import Lens.Micro.Aeson

class Monad m => Failing m where
  failing :: String -> m a

instance Failing (Either String) where
   failing = Left

instance Failing Maybe where
  failing = const Nothing

data Module = Module
  { _module_name :: Text
  , _module_description :: Maybe Text
  , _module_templates :: [Template]
  }
  deriving (Show, Typeable, Data)

data Template = Template
  { _template_name :: Text
  , _template_description :: Maybe Text
  , _template_choices :: [Choice]
  }
  deriving (Show, Typeable, Data)

data Choice = Choice
  { _choice_name :: Text
  , _choice_description :: Maybe Text
  , _choice_fields :: [Field]
  }
  deriving (Show, Typeable, Data)

data Field = Field
  { _field_name :: Text
  , _field_description :: Maybe Text
  }
  deriving (Show, Typeable, Data)

-- | Expects a json document produced by running @daml damlc docs -f json@
readModulesFromFile :: FilePath -> IO (Either String [Module])
readModulesFromFile f = do
  v <- eitherDecodeFileStrict f
  pure $ getModules =<< v

getModules :: Value -> Either String [Module]
getModules v = case v ^? _Array of
  Nothing -> Left "Input JSON should be an array"
  Just x -> mapM getModule $ toList x

get :: Failing m => Traversal' Value a -> String -> Text -> Value -> m a
get t input k v = case v ^? key k . t of
  Nothing -> failing (T.unpack k <> " not found when parsing " <> input)
  Just x -> return x

getArray :: Failing m => String -> Text -> Value -> m Array
getArray = get _Array

getString :: Failing m => String -> Text -> Value -> m Text
getString = get _String

getModule :: Value -> Either String Module
getModule v = do
  templates :: [Value] <- fmap toList $ getArray "Module" "md_templates" v
  name <- getString "Module" "md_name" v
  Module
    <$> pure name
    <*> pure (getString "Module" "md_descr" v)
    <*> sequence (getTemplate name <$> templates)

getTemplate :: Text -> Value -> Either String Template
getTemplate moduleName v = do
  choices :: [Value] <- fmap toList $ getArray "Template" "td_choices" v
  name <- getString ("Template in " <> T.unpack moduleName) "td_name" v
  Template
    <$> pure name
    <*> pure (getString "Template" "td_descr" v)
    <*> sequence (getChoice (moduleName <> "." <> name) <$> choices)

getChoice :: Text -> Value -> Either String Choice
getChoice templateName v = do
  fields :: [Value] <- fmap toList $ getArray "Choice" "cd_fields" v
  name <- getString ("Choice in " <> T.unpack templateName) "cd_name" v
  Choice
    <$> pure name
    <*> pure (getString "Choice" "cd_descr" v)
    <*> sequence (getField (templateName <> "." <> name) <$> fields)

getField :: Text -> Value -> Either String Field
getField choiceName v = do
  name <- getString ("Field in " <> T.unpack choiceName) "fd_name" v
  Field
    <$> pure name
    <*> pure (getString "Field" "fd_descr" v)
