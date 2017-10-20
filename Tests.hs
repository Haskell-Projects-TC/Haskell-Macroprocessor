module Tests where

import IC.TestSuite

import MP hiding (main)

lookUpTestCases
  = [ ("A", [("A", 8), ("B",9), ("C",5), ("A",7)]) ==> [8,7]
    , ("a", []) ==> []
    , ("a", [("a", 9)]) ==> [9]
    , ("a", [("b", 9)]) ==> []
    , ("!", [("H", 8), ("I",9), ("!",5), ("!",7)]) ==> [5,7]
    , ("5", [("2", 4), ("5",12), ("9",33), ("g",6)]) ==> [12]
    , ("hot", [("hot", 0), ("chocolate",71), ("9",86), ("%%",16)]) ==> [0]
    ] 

splitTestCases
  = [ (" .,", "A comma, then some words.")
        ==> (" ,   .",["A","comma","","then","some","words",""])
    , ("", "")
        ==> ("", [""])
    , (".", "A.B")
        ==> (".", ["A","B"])
    , (" ", " A")
        ==> (" ", ["", "A"])
    , ("\n\t.,:;!\"\'()<>/\\", "\n\t.,:;!\"\'()<>/\\")           
        ==> ("\n\t.,:;!\"\'()<>/\\",["","","","","","","","","","","","","","","",""])
    , ("", "helloworld")
        ==> ("", ["helloworld"])
    ]

combineTestCases
  = [ (" ,   .", ["A","comma","","then","some","words",""])
        ==> ["A"," ","comma",",",""," ","then"," ","some"," ","words",".",""]

    , ("", [""])
        ==> [""]
    , (".", ["A","B"])
        ==> ["A",".","B"]
    , (" ", ["", "A"])
        ==> [""," ","A"]
    , (" ;", ["hot","chocolate"])
        ==> ["hot"," ","chocolate",";"]
    ,  ("!", [""])    
         ==> ["","!"]
    ]

getKeywordDefsTestCases
  = [ ["$rule Reproduce this precisely -- or else!!"]
        ==> [("$rule","Reproduce this precisely -- or else!!")]
    , ["$x Define x", "$y 55"]
        ==> [("$x","Define x"),("$y","55")]
    , ["$a A", "$b B", "$c C"]
        ==> [("$a","A"),("$b","B"),("$c","C")]
    , []
        ==> []
    , ["$x-y-z $$$"]
        ==> [("$x-y-z","$$$")]
    , ["$$ something to think about"]
        ==> [("$$","something to think about")]
    , ["$ meanie!"]
        ==> [("$","meanie!")]
    , ["$var  Tristan Allwood"]
        ==> [("$var", " Tristan Allwood")]
    , ["$. Hello World."]
        ==> [("$.", "Hello World.")]
    , ["$hot choc  ;"]
        ==> [("$hot", "choc  ;")]
    ]

expandTestCases
  = [ ("The capital of $1 is $2", "$1 Peru\n$2 Lima.")
        ==> "The capital of Peru is Lima."
    , ("The time is $a", "$a now.")
        ==> "The time is now."
    , ("Keywords (e.g. $x, $y, $z...) may appear anwhere, e.g. <$here>.",
       "$x $a\n$y $b\n$z $c\n$here $this-is-one")
        ==> "Keywords (e.g. $a, $b, $c...) may appear anwhere, e.g. <$this-is-one>."
    , ("", "")
        ==> ""
    ]

allTestCases
  = [ TestCase "lookUp"  (uncurry lookUp)
                         lookUpTestCases
    , TestCase "split"   (uncurry split)
                         splitTestCases
    , TestCase "combine" (uncurry combine)
                         combineTestCases

    , TestCase "getKeywordDefs" getKeywordDefs
                                getKeywordDefsTestCases

    , TestCase "expand"  (uncurry expand)
                         expandTestCases
    ]

runTests = mapM_ goTest allTestCases

main = runTests
