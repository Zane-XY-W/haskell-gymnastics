{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module XML.UpdateXML where

import           Filesystem.Path (FilePath)
import           Prelude         hiding (FilePath, readFile, writeFile)
import           Text.XML
import           Text.XML.Cursor
import            Control.Applicative
import qualified Data.ByteString.Lazy as L

{--
find a location in the xml and then add/update that node.
eg xpath from //d/e/f and then change the content at 'f' or add a new node

<a>
...
  <d>
    <e>
      <f>some data to change</f>
    </e>
  </d>
...
</a>
--}
-- |
-- writeFile :: RenderSettings -> FilePath -> Document -> IO ()
-- readFile :: ParseSettings -> FilePath -> IO Document
-- fromDocument :: Document -> CursorSource , Convert a Document to a Cursor. It will point to the document root.
-- The operators $|, $/, $// and $.// can be used to apply an axis (right-hand side) to a cursor
-- ($/) :: Cursor node -> (Cursor node -> [a]) -> [a], Apply an axis to the children of a 'Cursor node'.

updateInplace :: FilePath -> FilePath -> IO()
updateInplace input output
  = do doc@(Document pro (Element name attrs _) epi) <- readFile def input
       let nodes = fromDocument doc $/ updateF
       writeFile def output $ Document pro (Element name attrs nodes) epi
  where updateF c -- ^ updateF :: Cursor node -> [Node]
          = case node c of
                NodeElement (Element "f" attrs _) | parentIsE c && gparentIsD c ->
                                                    [NodeElement $
                                                       Element "f" attrs
                                                         [NodeContent "New content"]]
                NodeElement (Element name attrs _) -> [NodeElement (Element name attrs  (c $/ updateF))]
                n -> [n]
        parentIsE c = not $ null $ parent c >>= element "e"
        gparentIsD c = not $ null $ parent c >>= parent >>= element "d"


updateNodeInplace :: L.ByteString -> L.ByteString
updateNodeInplace xmlStr
  = case parseLBS_ def xmlStr of
        doc@(Document pro (Element name attrs _) epi) ->
          let nodes = fromDocument doc $/ updateF' in
            renderLBS def {rsPretty = True} $ Document pro (Element name attrs nodes) epi
        _ -> L.empty
  where updateF' c
          = case node c of
                NodeElement (Element "f" attrs _) | parentIsE c && gparentIsD c ->
                                                    [NodeElement $ Element "f" attrs [NodeContent "New content"]]
                NodeElement (Element name attrs _) -> [NodeElement (Element name attrs (c $/ updateF'))]
                n -> [n]
        parentIsE c = not $ null $ parent c >>= element "e"
        gparentIsD c = not $ null $ parent c >>= parent >>= element "d"

-- ($//) :: Cursor node -> (Cursor node -> [a]) -> [a]
-- type Axis node = Cursor node -> [Cursor node]
-- data Cursor node = Cursor {...}
-- type Cursor = Cursor Node
-- node :: Cursor node -> node
updateNodeInplace' :: L.ByteString -> L.ByteString
updateNodeInplace' xmlStr
  = case parseLBS_ def xmlStr of
        doc@(Document pro (Element name attrs _) epi) ->
          let nodes = update $ fromDocument doc
          in renderLBS def {rsPretty = True} $ Document pro (Element name attrs nodes) epi
        _ -> L.empty
  where update rootCursor
          = case cursors of -- ^ update :: Cursor node -> [Node]
              [] -> [node rootCursor]
              (x:_) -> case node x of
                         NodeElement (Element "f" attrs _) -> [NodeElement (Element "f" attrs [NodeContent "New content"])]
                         n' -> [n']
          where cursors = rootCursor $// element "d" &/ element "e" &/ element "f"
                -- (rootCursor $// element "d") >>= ($/ element "e") >>= ($/ element "f") -- ^ \x -> x $/ element "f"

deepSelect :: Cursor -> Axis -> [Cursor]
deepSelect cur axis = cur $// axis


