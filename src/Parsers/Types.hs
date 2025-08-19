module Parsers.Types where

import Text.Megaparsec (Parsec)
import Data.Void (Void)

type Parser = Parsec Void String