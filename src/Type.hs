module Type where

import qualified MySet as S

type Alpha a = S.Set a
type States s = S.Set s
type Edge a s = (s, s, a)
type Edges a s = S.Set (Edge a s)
