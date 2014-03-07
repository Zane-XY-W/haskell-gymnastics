module Parser where
import qualified Data.ByteString.Lazy as LB
import           Data.Int

-- | remaining content and offset
data ParseState = ParseState{string :: LB.ByteString,  --  ^ residual
                             offset :: Int64           --  ^ offset
                            }
                deriving Show

-- | newtype renames T to N, here it renames Parser to a function
-- takes a ParseState returns a tuple, which is exactly a parser
-- newtype can only has one field label, which defines a 1 arity parameter
newtype Parser a -- ^ a : input type
  = Parser { runParser :: ParseState -> Either String (a, ParseState) -- ^ returning function
           }

-- | identity parser
identity :: a         --  ^ input type, the type of the input is irrelavent
         -> Parser a  --  result parser, also return a Parser of content type a
identity a = Parser ( \s -> Right (a, s)) -- ^  the type of Parser is take a lambda, returns a Parse

-- | a simple parser, doesn't transform input type
parseThrough :: Parser a         --  ^ the parser
      -> LB.ByteString    --  ^ raw input
      -> Either String a  --  ^  result
parseThrough  parser input =
        case runParser parser (ParseState input 0) of -- ^ newtype type is called by runX function
            Left err -> Left err
            Right (r, _) -> Right r
