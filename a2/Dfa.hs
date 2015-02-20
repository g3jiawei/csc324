{- Assignment 2 - Finite Automata (due November 11, noon)

Notes:
- You may import Data.List; you may not import any other modules

***Write the names and CDF accounts for each of your group members below.***
<Name>, <CDF>
<Name>, <CDF>
-}
module Dfa (State, Symbol, Transition, Automaton(..),
            allStrings, tableToDelta, extend, possibleOutcomes,
            accept, language, 
            removeUseless, isFiniteLanguage, language',epsilonClosure) where

import Data.List

-- Basic data types
type State = Integer
type Symbol = Char

type Transition = (State, Symbol, State)
tstate1 :: Transition -> State
tstate1 (s,_,_) = s
tstate2 :: Transition -> State
tstate2 (_,_,s) = s
tsymbol :: Transition -> Symbol
tsymbol (_,s,_) = s

-- Automaton Data Type
-- Automaton states alphabet transitions initial final
data Automaton = Automaton [State] [Symbol] [Transition] State [State]
-- Some helper functions for you to access the different automaton components
states :: Automaton -> [State]
states (Automaton s _ _ _ _) = s
alphabet :: Automaton -> [Symbol]
alphabet (Automaton _ a _ _ _) = a
transitions :: Automaton -> [Transition]
transitions (Automaton _ _ ts _ _) = ts
initial :: Automaton -> State
initial (Automaton _ _ _ i _) = i
final :: Automaton -> [State]
final (Automaton _ _ _ _ f) = f

-- Questions 1-4: transitions
tableToDelta :: [Transition] -> State -> Symbol -> [State]
tableToDelta trans state sym =sort (nub (map (\s -> tstate2 s) (filter (\x -> ((tstate1 x) == state && (tsymbol x) == sym) ) trans)))

map1 f l = foldl(\a b -> a ++ b) [] (map f l)

extend :: (State -> Symbol -> [State]) -> (State -> String -> [State])
extend transf state string = sort(nub (foldl (\x s -> if (null (map1 (\z -> transf z s) x)) then [] else (map1 (\z -> transf z s) x)) [state] string))

t = [(0,'a',1),(0,'a',2),(0,'a',3),(0,'a',4),(4,'a',7),(7,'a',9),(5,'a',5)]
f x y = tableToDelta [(0,'a',1),(0,'a',2),(0,'a',3),(0,'a',4),(4,'a',4),(4,'b',7),(3,'a',1),(2,'a',1),(1,'a',1),(7,'b',7)] x y

appendToString :: String -> [Symbol] -> [String]
appendToString s c = map(\x -> s ++ [x]) c

allStrings :: [Symbol] -> [[String]]
allStrings s = [""]:[map1 (\y -> appendToString y (sort s)) x | x <-allStrings s]

possibleOutcomes :: Automaton -> State -> [[(String, [State])]]
possibleOutcomes a s = map (\x-> helper s x a) (allStrings (alphabet a))

helper state string automaton = map (\x-> (x,extend (tableToDelta (transitions automaton)) state x)) string

ex = Automaton [0,1,2,8] 
               ['a','b']
               [(0,'a',1),
                (0,'b',2),
                (1,'a',1),
                (1,'b',2),(2, 'a',1),(2,'b',2),(8,'a',8),(8,'b',8)]
               0 
               [0,2]
inList :: Eq x => x -> [x] -> Bool
inList x y = foldl (\c s -> (x == s) || c) False y

setIntersect :: Eq a => [a] -> [a] -> Bool
setIntersect a b = foldl(\s x ->if (inList x b) then True||s else False||s) False a

-- Questions 5-6: acceptance
accept :: Automaton -> String -> Bool
accept a str =setIntersect (extend (tableToDelta (transitions a)) (initial a) str) (final a)
language :: Automaton -> [String]
language a  = filter (\x -> accept a x)(languageh a) 
languageh a = foldr (++) [] ((allStrings (alphabet a)))


-- Questions 7-9: finiteness

numStates :: Automaton -> Int
numStates a = length (states a)
stringsWithMaxLength n a = foldl(\a b -> a ++ b) [] (take (n+1) (allStrings (alphabet a)))

--returns true if we end up in final state
getFinalState :: State -> String -> Automaton -> Bool
getFinalState initial str a = setIntersect (extend (tableToDelta (transitions a)) initial str)
						(final a)
isStateUseful :: State -> Automaton -> Bool
isStateUseful state a = foldl (\s x -> if (getFinalState state x a) then True||s else False||s) 
			 False (stringsWithMaxLength (numStates a) a)

getUselessStates :: Automaton -> [State]
getUselessStates a = foldl (\s x -> if ((isStateUseful x a) == False)
                                    then s ++ [x]
				    else s) [] (states a)

removeStatesFromAutomaton :: Automaton -> [State]
removeStatesFromAutomaton a = foldl (\s x -> if(inList x (getUselessStates a))
					     then s
                                             else s ++ [x]) [] (states a)
isSubset:: Eq a => [a] -> [a] -> Bool
isSubset a b = foldl(\s x ->if (inList x b) then True&&s else False&&s) True a

removeInvalidTransitions :: Automaton -> [Transition]
removeInvalidTransitions a = foldl (\s x -> if (isSubset [(tstate1 x),(tstate2 x)]          
						(removeStatesFromAutomaton a))
					      then s ++ [x]
					      else s) [] (transitions a)

removeInvalidFinalStates :: Automaton -> [State]
removeInvalidFinalStates a = foldl (\s x -> if(inList x (getUselessStates a))
					     then s
                                             else s ++ [x]) [] (final a)

removeUseless :: Automaton -> Automaton
removeUseless a = Automaton (removeStatesFromAutomaton a)
                               (alphabet a)
				(removeInvalidTransitions a)
				(initial a)
				(final a)

getStringsNPlus1Length  :: Automaton -> [[Char]]
getStringsNPlus1Length a = filter (\x -> accept a x)((allStrings (alphabet a)) !! ((numStates a)+1))


isFiniteLanguage :: Automaton -> Bool
isFiniteLanguage a = foldl (\s x -> if(accept (removeUseless a) x)
				    then False else True&&s)
                                    True (getStringsNPlus1Length (removeUseless a))
language' :: Automaton -> [String]
language' a = if (isFiniteLanguage a) then (filter (\x -> accept a x)(stringsWithMaxLength ((numStates a)+1) a))
		else language a

a3 = Automaton [0,1,2,3,4,5] ['a','b'] 
		[(0,' ',0),(0,' ',1),(0,' ',4),(1,' ',2),(2,' ',3),(2,'a',3),(4,'b',5)] 
		0 [3,5]

-- Question 10: epsilon transitions

getAllETransitions a = filter(\x -> (tsymbol x) == ' ') (transitions a)
createEmptyStrings = "": map ( ++ " ") createEmptyStrings
emptyStringsN n = take n (createEmptyStrings)

epsilonClosure :: Automaton -> [State]-> [State]
epsilonClosure a s = (sort. nub)((concatMap (\y -> (concatMap (\x -> extend (tableToDelta (transitions a)) y x) (emptyStringsN (numStates a)))) s))

q10helper a s size = undefined
