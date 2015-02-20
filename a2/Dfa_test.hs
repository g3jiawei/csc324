{- Sample tests for Assignment 2 -}
import Test.HUnit
import Dfa (State, Symbol, Transition, Automaton(..),
            allStrings, tableToDelta, extend, possibleOutcomes,
            accept, language,
            removeUseless, isFiniteLanguage, language', epsilonClosure)


tableToDeltaTests = TestList [
    [2] ~=? tableToDelta [(1, 'f', 2)] 1 'f',
    -- Note: a symbol could be passed in that doesn't appear in any transition
    [2,3] ~=? tableToDelta [(1, 'a', 2),(1, 'a', 3)] 1 'a',
    [] ~=? tableToDelta [(1, 'f', 2)] 2 'f',
    [1] ~=? tableToDelta [(1, 'a', 1),(1, 'a', 1)] 1 'f'
    ]

extendTests = TestList [
    [2] ~=? extend (tableToDelta [(1, 'f', 2), (2, 'f', 2)]) 1 "ff",
    [1] ~=? extend (tableToDelta []) 1 "f",
    [2] ~=? extend (tableToDelta [(1, 'f', 2), (2, 'f', 2)]) 1 "fff"
    ]

allStringsTests = TestList [
    ["aa", "ab", "ba", "bb"] ~=? allStrings "ab" !! 2,
    [""] ~=? allStrings "ab" !! 0,
    ["aa"] ~=? allStrings "a" !! 2,
    [] ~=? allStrings "" !! 5 
    ]

possibleOutcomesTests = TestList [
    [("aa",[1]), ("ab",[0,2]), ("ba",[0,2]), ("bb",[1])] ~=?
        (possibleOutcomes (Automaton [0,1,2]
                                     ['a','b']
                                     [(0,'a',1),
                                      (1,'a',2),
                                      (0,'b',0),
                                      (1,'b',1),
                                      (2,'b',2),
                                      (1,'a',0)] 0 [2]) 1) !! 2,
    [] ~=?
        (possibleOutcomes (Automaton []
                                     []
                                     [] 0 [2]) 1) !! 3,
    [("aaa",[0,2]), ("aab",[1]), ("aba",[1]), ("abb",[0,2]),("baa",[1]),("bab",[0,2]),("bba",[0,2]),("bbb",[1])] ~=?
        (possibleOutcomes (Automaton [0,1,2]
                                     ['a','b']
                                     [(0,'a',1),
                                      (1,'a',2),
                                      (0,'b',0),
                                      (1,'b',1),
                                      (2,'b',2),
                                      (1,'a',0)] 0 [2]) 1) !! 3,
    [("aa",[2]), ("ab",[2]), ("ba",[2]), ("bb",[2])] ~=?
        (possibleOutcomes (Automaton [0,1,2]
                                     ['a','b']
                                     [(0,'a',1),
                                      (1,'a',2),
                                      (0,'b',0),
                                      (1,'b',1),
                                      (2,'b',2),
                                      (1,'a',0)] 0 [2]) 2) !! 2
    ]

a1 = Automaton [0,1] ['a'] [(0,'a',1),(1,'a',0)] 0 [0]

acceptTests = TestList [
    True ~=? accept a1 "aa"
    ]

languageTests = TestList [
    ["","aa"] ~=? take 2 (language a1)
    ]

a2 = Automaton [0,1] ['a'] [(0,'a',1)] 0 [0]

eq :: Automaton -> Automaton -> Bool
eq (Automaton s1 a1 ts1 i1 f1) (Automaton s2 a2 ts2 i2 f2) =
    s1 == s2 &&
    a1 == a2 &&
    ts1 == ts2 &&
    i1 == i2 &&
    f1 == f2

removeUselessTests = let a3 = removeUseless a2
    in
    TestList [
        True ~=? eq a3 (Automaton [0] ['a'] [] 0 [0])
        ]

isFiniteLanguageTests = TestList [
    True ~=? isFiniteLanguage a2
    ]


language'Tests = TestList [
    [""] ~=? language' a2
    ]

a3 = Automaton [0,1,2] ['a','b'] [(0,' ',2),(0,'a',1),(2,'b',0)] 0 [1]


epsilonClosureTests = TestList [
    [0,2] ~=? epsilonClosure a3 [0]
    ]

main :: IO ()
main = do
    -- Put each call to "runTestTT" on a separate line
    runTestTT tableToDeltaTests
    runTestTT extendTests
    runTestTT allStringsTests
    runTestTT possibleOutcomesTests
    runTestTT acceptTests
    runTestTT languageTests
    runTestTT removeUselessTests
    runTestTT isFiniteLanguageTests
    runTestTT language'Tests
    runTestTT epsilonClosureTests
    return ()
