module Mini.Compiler.Parser.Error (Error (..)) where

data Error = Error
    { reason :: String
    , currentInput :: String
    }
