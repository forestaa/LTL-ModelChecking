module Kripke where

import qualified Buchi as B
import qualified MySet as S
import Type
import Data.Maybe

data Kripke = Kripke {states :: States String, initial :: String, relations :: S.Set (String, String), labeling :: S.Set (String, S.Set String)} deriving Show

kripkeToBuchi :: Kripke -> B.Buchi (S.Set String) String
kripkeToBuchi k
  | S.member Nothing edges = error "There is a no-label state"
  | otherwise = B.Buchi a s s0 (fromJust <$> edges) f
  where
    a = S.powerset $ labeling k >>= snd
    s = states k
    s0 = S.singleton $ initial k
    edges = S.map (\(s,s') -> (\label -> (s,s',label)) <$> label s') (relations k)
    label s = snd <$> S.satisfyingElement (\(s',_) -> s == s') (labeling k)
    f = states k
