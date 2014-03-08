module Parser where
import           Control.Applicative
import qualified Data.ByteString.Lazy as LB
import           Data.Char
import           Data.Int
import           Data.Word8

-- | remaining content and offset
data ParseState = ParseState{string :: LB.ByteString,  --  ^ residual
                             offset :: Int64           --  ^ offset
                            }
                deriving Show

-- | newtype renames T to N, here it renames Parser to a function
-- takes a ParseState returns a tuple, which is exactly a parser
-- newtype can only has one field label, which defines a 1 arity parameter
newtype Parser a -- ^ a : input/result type
  = Parser { runParser :: ParseState
                       -> Either String (a, ParseState) -- ^ a is the intermediate result
           }

-- | identity parser
identity :: a         --  ^ input type, the type of the input is irrelavent
         -> Parser a  --  result parser, also return a Parser of content type a
identity a = Parser (\s -> Right (a, s)) -- ^  the type of Parser is take a lambda, returns a Parse

-- | a simple parser, doesn't transform input type
parseThrough :: Parser a         -- ^ the parser
             -> LB.ByteString    -- ^ raw input
             -> Either String a  -- ^ result
parseThrough  parser input =
  case runParser parser (ParseState input 0) of -- ^ newtype type is called by runX function
      Left err -> Left err
      Right (r, _) -> Right r

-- | pa parser a, pb parser b, pn parser new
-- call parse function on pa, and if succeed, pass it to pb, get a pn
(==>) :: Parser a -- ^ parser of input type a
      -> (a -> Parser b) -- ^  the reuslt of pa
      -> Parser b
pa ==> pb = Parser pn
  where pn inputState =
          case runParser pa inputState of
            Left e -> Left e
            Right (r, newState) -> runParser (pb r) newState

-- | inject error msg into parser result
bail :: String
     -> Parser a
bail err = Parser (\s -> Left ("at: " ++ show (offset s) ++ " with error " ++ err))

-- | the final result is contained in ParseState, because the intermediate
-- reuslt is in first element of the tuple, you can think this \s -> (s, s)
-- as write s to the first of the tuple.
finalParserState :: Parser ParseState -- ^ result is ParseState
finalParserState = Parser (\s -> Right (s, s))

-- | Parser ()
buildState:: ParseState
          -> Parser () -- ^ a new State doesn't contains intermediate result
buildState s = Parser (\_ -> Right((), s))

-- | m a ==> a -> m b -> m b
-- useless and contrived example
parseByte :: Parser Word8
parseByte = finalParserState ==> \initState ->
                   case LB.uncons (string initState) of
                     Nothing -> bail "end of input"
                     Just (byte, residual) -> buildState newState ==> \_ -> -- a -> m b here
                        identity byte -- stops here
                        where newState = initState {string = residual,
                                                   offset = newOffset}
                              newOffset = offset initState + 1
-- | Functor definition
instance Functor Parser where
        fmap f parser = parser ==> \r -> identity (f r) -- unpack a parser by applying it to ==>

word2Char :: Word8 -> Char
word2Char = chr . fromIntegral

-- |  quick definition of parseChar from parseByte
parseChar :: Parser Char
parseChar = word2Char <$> parseByte

-- | in previous Parser functor definition, fmap f parser, f applies to the
-- r, so here f <$> finalParserState, f also directly applies to r, don't
-- need worry about that steps, you only need to think about how to write a
-- f, takes r returns a Maybe Word8, and in the finalParserState, r is a
-- ParseState
peekByte :: Parser (Maybe Word8)
peekByte  = (fmap fst . LB.uncons . string) <$> finalParserState

peekChar :: Parser (Maybe Char)
peekChar = fmap word2Char <$> peekByte

parseWhile:: (Word8 -> Bool)
          -> Parser [Word8]
parseWhile predicate =  peekByte ==> \r ->
    case fmap predicate r of
        Nothing -> identity []
        Just True -> parseByte ==> \b -> (b:) <$> parseWhile predicate
