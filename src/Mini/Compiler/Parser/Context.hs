module Mini.Compiler.Parser.Context (Context (..)) where

import Mini.Typeclass.New (New (new))

data Context = Context
    { indentationLevel :: Int
    }

instance New Context where
    new = Context{indentationLevel = 0}

