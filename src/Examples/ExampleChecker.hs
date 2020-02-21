module Examples.ExampleChecker where

import Types.Program
import Types.Type
import Types.Environment
import Synquid.Error
import Synquid.Parser
import HooglePlus.Refinement (solveTypeConstraint)

import Text.Parsec.Indent

type Example = [RProgram]

parseExample :: [String] -> Example
parseExample [] = []
parseExample (ex:exs) = parse ex : parseExample exs
    where
        parse = runIndentParserT parseProgram () ""

checkExample :: Environment -> RType -> Example -> Either ErrorMessage Example
checkExample env typ@(ScalarT {}) [ex] = typeOf ex
