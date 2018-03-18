module Main where

import MySet as S
import Parser
import Buchi as B
import Rabin as R
import LTL
import Kripke as K

main :: IO ()
main = complementtest

modelcheckingtest = do
  putStrLn ""
  putStrLn "----------------------------------"
  putStrLn "Kripke structure is"
  print kripke
  putStrLn "LTL formula is"
  print ltl
  putStrLn "--------------------------------------------"
  putStrLn "making intersection Buchi..."
  let b = K.kripkeToBuchi kripke
      b' = ltlToBuchiOnKripke (Not ltl) kripke
      b'' = B.intersectionOfTwoBuchis b b'
  putStrLn "emptiness checking..."
  B.isempty b''
  where
    kripke = K.Kripke (S.fromList ["1","2","3","4","5","6","7"]) "1" (S.fromList [("1","2"),("1","3"),("2","5"),("3","1"),("3","6"),("4","1"),("4","3"),("5","2"),("5","3"),("6","7"),("7","4")]) (S.fromList [("1",S.empty),("2",S.fromList ["Start","Error"]),("3",S.fromList ["Close"]),("4",S.fromList ["Close","Heat"]),("5",S.fromList ["Start","Close","Error"]),("6",S.fromList ["Start","Close"]),("7",S.fromList ["Start","Close","Heat"])])
    ltl = global $ LAP "Start" `implication` future (LAP "Heat")


complementtest = do
  putStrLn ""
  putStrLn "---------------------------------"
  putStrLn "Does b' include b?"
  putStrLn "emptiness checking..."
  B.isempty $ B.intersectionOfTwoBuchis b (R.complementOfBuchi b')
  putStrLn "Does b include b'?"
  putStrLn "emptiness checking..."
  B.isempty $ B.intersectionOfTwoBuchis (R.complementOfBuchi b) b'
  where
    ltl = global (future (LAP 'p') `implication` LAP 'q')
    b = R.complementOfBuchi $ ltlToBuchi ltl
    b' = ltlToBuchi $ Not ltl

test = do
  putStrLn ""
  putStrLn "---------------------------------"
  putStrLn "My Buchi automaton satisfy the LTL?"
  let b' = ltlToBuchi $ Not ltl
      b'' = B.intersectionOfTwoBuchis b b'
  putStrLn "emptiness checking..."
  B.isempty b''
  where
    ltl = Not (LAP 'p') `U` X (LAP 'q')
    b = B.Buchi (S.powerset $ S.fromList "pq") (S.fromList [0,1,2]) (S.singleton 0) (S.fromList [(0,0,S.empty),(0,0,S.singleton 'q'),(0,1,S.empty),(0,1,S.singleton 'p'),(0,1,S.singleton 'q'),(0,1,S.fromList "pq"),(1,2,S.singleton 'q'),(1,2,S.fromList "pq"),(2,2,S.empty),(2,2,S.singleton 'p'),(2,2,S.singleton 'q'),(2,2,S.fromList "pq")]) (S.singleton 2)
