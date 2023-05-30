module Hatch.Decode where

import Hatch.Types

import qualified Data.Aeson as JSON
import Chronos
import Cleff
import qualified Data.Text as T

decode :: ACH -> T.Text
decode = decode

decodeJSON :: ACH -> JSON.Value
decodeJSON = decodeJSON
