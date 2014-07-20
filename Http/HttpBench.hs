module HttpBench where
import           Control.Applicative
import           Control.Monad         (void)
import           Criterion.Measurement
import           Network.HTTP
import           Network.HTTP.Conduit  (simpleHttp)

benchURL :: String
benchURL = "http://www.google.com/intl/en/policies/privacy/index.html"

-- | http-conduit package
httpConduitBench :: IO String
httpConduitBench = secs <$> time_ (void $ simpleHttp benchURL)

-- | HTTP package
httpBench:: IO String
httpBench = secs <$> time_ (void $ getResponseBody =<< simpleHTTP (getRequest benchURL))

main :: IO ()
main =  sequence [httpConduitBench
                 , httpBench] >>= print

