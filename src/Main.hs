{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Array as A
import qualified Data.Graph as G
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T

import           BasePrelude
import           Data.Aeson
import           Data.Graph (Vertex, Graph)
import           Data.Tree

newtype Links = Links [(T.Text, T.Text)] deriving (Show)
data Component a = Component Graph (Tree a) (S.Set a) (a -> String)

instance FromJSON Links where
  parseJSON (Object o) = do
    children <- o .: "links"
    links <- mapM (liftA2 (,) <$> (.: "source") <*> (.: "target")) children
    return (Links links)
  parseJSON _ = undefined

compose :: Ord a => Graph -> (a -> String) -> Tree a -> Component a
compose g f t = Component g t (fold (S.singleton <$> t)) f

scc :: Links -> [Component Vertex]
scc (Links links) =
  [compose graph (T.unpack . nameOf) tree | tree <- G.scc graph]
  where
    grouped = M.fromListWith (<>) ((\(s, t) -> (s, [t])) <$> links)
    (graph, f) = G.graphFromEdges' ((\(s, ts) -> (s, s, ts)) <$> M.toList grouped)
    nameOf = (\(x, _, _) -> x) . f

present :: (Show a) => a -> T.Text
present = T.pack . show

intersection :: Ord a => [a] -> S.Set a -> [a]
intersection xs ys =
  S.toList (S.intersection (S.fromList xs) ys)

pretty :: Component Vertex -> Tree String
pretty (Component g tree keys toString) = f tree
  where
    f (Node a []) =
      Node (toString a <> " -> " <> show (toString <$> intersection (g A.! a) keys)) []
    f (Node a forest) =
      Node (toString a) (f <$> forest)

main :: IO ()
main = do
  links <- toLinks <$> BL.readFile "nodes.js"
  let components = scc links
  putStrLn (drawForest $ pretty <$> components)
  where
    toLinks :: BL.ByteString -> Links
    toLinks = fromJust . decode . BL.drop (fromIntegral $ T.length "graph=")
