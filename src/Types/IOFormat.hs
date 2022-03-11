module Types.IOFormat where

import GHC.Generics
import Data.Aeson
import Data.Serialize
import Data.Data
import Data.Text ( Text )
import Control.DeepSeq

import Types.Type

outputPrefix :: Text
outputPrefix = "RESULTS:"

data QueryType = SearchPrograms
               | SearchTypes
               | SearchResults
               | SearchExamples
  deriving(Eq, Data, Show, Generic)

instance FromJSON QueryType
instance ToJSON QueryType

data Example = Example { inputs :: [String]
                       , output :: String
                       }
  deriving(Eq, Ord, Generic)

instance Show Example where
    show e = unwords [unwords (inputs e), "==>", output e]

instance ToJSON Example
instance FromJSON Example
instance Serialize Example

type ErrorMessage = String
type TypeQuery = String

data QueryInput = QueryInput { query :: TypeQuery
                             , inExamples :: [Example]
                             , inArgNames :: [String]
                             }
  deriving(Eq, Show, Generic)

instance FromJSON QueryInput
instance ToJSON QueryInput

data FunctionDoc = FunctionDoc { functionName :: String
                               , functionSig :: String
                               , functionDesc :: String
                               }
  deriving(Eq, Show, Generic, NFData)

instance ToJSON FunctionDoc

data ResultEntry = ResultEntry { qualSolution :: String
                               , unqualSolution :: String
                               , outExamples :: [Example]
                               }
  deriving(Eq, Show, Generic)

instance ToJSON ResultEntry

data QueryOutput = QueryOutput { outCandidates :: [ResultEntry]
                               , outError :: String
                               , outDocs :: [FunctionDoc]
                               }
  deriving(Eq, Show, Generic)

instance ToJSON QueryOutput

data ExecInput = ExecInput { execQuery :: TypeQuery
                           , execArgs :: [String]
                           , execProg :: String
                           }
  deriving(Eq, Show, Generic)

instance FromJSON ExecInput

data ExecOutput = ExecOutput { execError :: String
                             , execResult :: String
                             }
  deriving(Eq, Show, Generic)

instance ToJSON ExecOutput

data ExamplesInput = ExamplesInput { exampleQuery :: TypeQuery
                                   , exampleProgram :: String
                                   , exampleExisting :: [Example]
                                   }
  deriving(Eq, Show, Generic)

instance FromJSON ExamplesInput

data ListOutput a = ListOutput { examplesOrTypes :: [a]
                               , tqError :: String
                               }
  deriving(Eq, Generic)

instance ToJSON a => ToJSON (ListOutput a)
