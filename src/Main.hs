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
data Component a = Component Graph (a -> String) (S.Set a) (Tree a)

instance FromJSON Links where
  parseJSON (Object o) = do
    children <- o .: "links"
    links <- mapM (liftA2 (,) <$> (.: "source") <*> (.: "target")) children
    return (Links links)
  parseJSON _ = undefined

-- Neat trick: transmogrity `Foldable f => f a` into `f (Set a)`, then fold, to get `Set a`.
compose :: Ord a => Graph -> (a -> String) -> Tree a -> Component a
compose g f t = Component g f (fold . fmap S.singleton $ t) t

scc :: Links -> [Component Vertex]
scc (Links links) =
  [compose graph (T.unpack . nameOf) tree | tree <- G.scc graph]
  where
    -- This could be better, but Graph has a gnarly tuple-based API and I'm rushing.
    grouped = M.fromListWith (<>) ((\(s, t) -> (s, [t])) <$> links)
    (graph, f) = G.graphFromEdges' ((\(s, ts) -> (s, s, ts)) <$> M.toList grouped)
    nameOf = (\(x, _, _) -> x) . f

intersection :: Ord a => [a] -> S.Set a -> [a]
intersection xs ys =
  S.toList (S.intersection (S.fromList xs) ys)

pretty :: Component Vertex -> Tree String
pretty (Component g toString keys tree) = f tree
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
