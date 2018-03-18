module Main where


import qualified Buchi          as B
import           Kripke
import           LTL
import qualified MySet          as S
import           Parser
import           System.Process
import           Type

main :: IO ()
main = do
  putStrLn "input Kripke structure"
  kripke <- getLine >>= getKripke
  putStrLn "input LTL formula"
  ltl <- getLine >>= getLTL
  putStrLn "--------------------------------------------"
  putStrLn "making intersection Buchi..."
  let b_k   = kripkeToBuchi kripke
      b_ltl = ltlToBuchiOnKripke (Not ltl) kripke
      b_int = B.intersectionOfTwoBuchis b_k b_ltl
  putStrLn "emptiness checking..."
  B.isempty b_int
  putStrLn "output: Kripke structure and LTL formula at ./images/*.png"
  writeBuchiOnDot "images/kripke.dot" b_k
  system "dot -Tpng -o images/kripke.png images/kripke.dot"
  writeBuchiOnDot "images/ltl.dot" $ ltlToBuchiOnKripke ltl kripke
  system "dot -Tpng -o images/ltl.png images/ltl.dot"
  return ()


writeBuchiOnDot :: (Show a, Show s, Ord a, Ord s) => String -> B.Buchi a s -> IO ()
writeBuchiOnDot output b =
  writeFile output $ concat [prefix, "  // node define\n", start, nodes, finalnodes, "\n  // edge define\n", initialedges, edges, "\n}"]
  where
    prefix = "digraph graph_name {\n  graph [\n    charset = \"UTF-8\";\n    layout = circo\n  ];\n\n"
    start = "  \"start\" [shape = plaintext];\n"
    nodes = S.foldr (\s t -> concat ["  \"", show' s, "\" [shape = circle];\n", t]) "" $ B.states b S.\\ B.final b
    finalnodes = S.foldr (\s t -> concat ["  \"", show' s, "\" [shape = doublecircle];\n", t]) "" $ B.final b
    initialedges = S.foldr (\s t -> concat ["  \"start\" -> \"", show' s, "\";\n", t]) "" . B.initial $ b
    edges = S.foldr (\(s,s',a) t -> concat ["  \"", show' s, "\" -> \"", show' s', "\" [label = \"", show' a, "\"];\n", t]) "" . unifyedges . B.transition $ b

unifyedges :: (Ord a, Ord s) => S.Set (Edge a s) -> S.Set (Edge (S.Set a) s)
unifyedges = S.foldr f S.empty
  where
    f (s,s',a) edges =
      let (l,r) = S.partition (\(t,t',_) -> s == t && s' == t') edges
        in
        if S.null l then S.insert (s,s',S.singleton a) r else
          let (_,_,as) = S.findMin l in S.insert (s,s',S.insert a as) r

show' :: Show a => a -> String
show' = filter ('"' /= ) . show
