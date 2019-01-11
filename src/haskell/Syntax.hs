module Syntax where

-- This module defines parsers for syntax used throughout the Sigma16
-- system.

import Data.Word
import Data.Bits
import Data.List
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Token
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Error
import Text.ParserCombinators.Parsec.Language

import Arithmetic
import Architecture
