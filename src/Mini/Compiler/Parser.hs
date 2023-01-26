module Mini.Compiler.Parser (Parser (..), defaultErrMsg) where

import Mini.Compiler.Parser.Error (Error (Error))
import Control.Applicative (Alternative)
import GHC.Base (Alternative(..))

defaultErrMsg :: String
defaultErrMsg = "Syntax Error"

type ParseResult c x = Either Error (String, c, x)

newtype Parser c x = Parser
    { parse :: String -> c -> ParseResult c x
    }

_applyToOutput :: (ParseResult c x -> ParseResult c y) -> Parser c x -> Parser c y
_applyToOutput f (Parser parseA) = Parser $ (fmap . fmap) f parseA

_applyToSuccessfulOutput :: (String -> c -> x -> ParseResult c y) -> Parser c x -> Parser c y
_applyToSuccessfulOutput f = _applyToOutput (\e -> e >>= \(a, b, c) -> f a b c)

instance Functor (Parser c) where
    fmap f (Parser p) = Parser $ (fmap . fmap . fmap . fmap) f p

instance Applicative (Parser c) where
    pure x = Parser{parse = \input context -> Right (input, context, x)}

    a <*> (Parser parseB) =
        _applyToSuccessfulOutput
            ( \input context parsedFunc ->
                (fmap . fmap) parsedFunc (parseB input context)
            )
            a

instance Monad (Parser c) where
    a >>= f =
        _applyToSuccessfulOutput
            ( \input context value ->
                parse (f value) input context
            )
            a

instance MonadFail (Parser c) where
    fail msg = Parser $ \s _ -> Left (Error msg s)

instance Semigroup a => Semigroup (Parser c a) where
    a <> (Parser parseB) =
        _applyToSuccessfulOutput
            ( \input context valueA ->
                (fmap . fmap) (valueA <>) (parseB input context)
            )
            a

instance Monoid a => Monoid (Parser c a) where
    mempty = pure mempty

instance Alternative (Parser c) where
    empty = fail defaultErrMsg
    (Parser parseF) <|> (Parser parseG) = Parser $ \s c ->
        parseF s c <> parseG s c

