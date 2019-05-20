{-# LANGUAGE ScopedTypeVariables #-}  -- Включаем некоторые расширения компилятора.
import Test.Tasty
import Test.Tasty.HUnit
import Data.Proxy
import Map
import qualified Data.Map.Strict as SMap
import MapInstance
import NaiveList(NaiveList)  -- Импортируем только тип NaiveList, но не его конструкторы Nil/Cons, чтобы не путались с конструкторами NaiveTree.
import NaiveTree

main :: IO ()
main = defaultMain testMap

{-|
  Генерирует группу тестов для конкретной реализации 'Map'
  с определённым именем.

  Мы хотим писать тесты один раз для всех возможных реализаций 'Map'.
  В чистом Haskell нам может помочь параметрический полиморфизм,
  но для этого нужно, чтобы в сигнатуре функции присутствовал
  тип из класса 'Map', который мы хотим протестировать.

  Специально для этих целей существует обёртка 'Data.Proxy', он
  позволяет передавать в функции даже типы высшего порядка.
-}
mapTests :: Map m => String -> Proxy m -> TestTree
mapTests name (_ :: Proxy m) =
    -- Чтобы можно было связать типовую переменную m здесь и в let ниже, нужно расширение ScopedTypeVariables.
    testGroup name [
        testCase "empty" $
            let tr = empty :: m Int String in
                Map.null tr @?= True,

        testCase "singleton" $
            let tr = singleton 3 "c" :: m Int String in do {
                size tr         @?= 1;
                Map.lookup 3 tr @?= Just "c"
            },

        testGroup "fromList" [
            testCase "Empty list" $
                let tr = fromList [] :: m Int Int in
                Map.null tr @?= True,

            testCase "Non-empty list" $
                let tr = fromList [(1, 10), (2, 100), (3, 1000), (1, 50)] :: m Int Int in do
                {
                    Map.lookup 1 tr @?= Just 50;
                    Map.lookup 2 tr @?= Just 100;
                    Map.lookup 3 tr @?= Just 1000;
                    size tr         @?= 3
                }
        ],

        testGroup "toAscList" [
            testCase "Empty list" $
                let tr = fromList [] :: m Int String in
                toAscList tr @?= [],
                
            testCase "toAscList . fromList sorts list" $
                let tr = fromList [(2, "a"), (1, "b"), (3, "c"), (1, "x")] :: m Int String in
                toAscList tr @?= [(1, "x"), (2, "a"), (3, "c")]
        ],

        testGroup "insert" [
            testCase "Adding new element" $
                let tr  = fromList [("a", "x"), ("b", "y"), ("c", "z")] :: m String String
                    tr' = insert "d" "xyz" tr in do {
                    size tr'           @?= 4;
                    Map.lookup "a" tr' @?= Just "x";
                    Map.lookup "b" tr' @?= Just "y";
                    Map.lookup "c" tr' @?= Just "z";
                    Map.lookup "d" tr' @?= Just "xyz"
                },

            testCase "Replacing existing element" $
                let tr  = fromList [("a", "x"), ("b", "y"), ("c", "z")] :: m String String
                    tr' = insert "a" "zyx" tr in do {
                    size tr'           @?= 3;
                    Map.lookup "a" tr' @?= Just "zyx";
                    Map.lookup "b" tr' @?= Just "y";
                    Map.lookup "c" tr' @?= Just "z"
                }
        ],
        
        testGroup "insertWith" [
            testCase "Adding new element" $
                let tr  = fromList [(1, 8), (2, 7)] :: m Int Int
                    tr' = insertWith (-) 3 6 tr in do {
                    size tr'         @?= 3;
                    Map.lookup 1 tr' @?= Just 8;
                    Map.lookup 2 tr' @?= Just 7;
                    Map.lookup 3 tr' @?= Just 6
                },

            testCase "Changing existing element" $
                let tr  = fromList [(1, 8), (2, 7)] :: m Int Int
                    tr' = insertWith (-) 1 6 tr in do {
                    size tr'         @?= 2;
                    Map.lookup 1 tr' @?= Just (-2);
                    Map.lookup 2 tr' @?= Just 7
                }
        ],

        testGroup "insertWithKey" [
            testCase "Adding new element" $
                let f k v' v = (show k) ++ ": " ++ v ++ "->" ++ v'
                    tr  = fromList [(1, "aa"), (2, "bb")] :: m Int String
                    tr' = insertWithKey f 3 "cc" tr in do {
                    size tr'         @?= 3;
                    Map.lookup 1 tr' @?= Just "aa";
                    Map.lookup 2 tr' @?= Just "bb";
                    Map.lookup 3 tr' @?= Just "cc"
                },

            testCase "Changing existing element" $
                let f k v' v = (show k) ++ ": " ++ v ++ "->" ++ v'
                    tr  = fromList [(1, "aa"), (2, "bb")] :: m Int String
                    tr' = insertWithKey f 1 "cc" tr in do {
                    size tr'         @?= 2;
                    Map.lookup 1 tr' @?= Just "1: aa->cc";
                    Map.lookup 2 tr' @?= Just "bb"
                }
        ],
        
        testGroup "delete" [
            testCase "Existing element" $
                let tr  = fromList [(1, "a"), (2, "b")] :: m Int String
                    tr' = delete 2 tr in do {
                    size tr'         @?= 1;
                    Map.lookup 1 tr' @?= Just "a"
                },

            testCase "Absent element" $
                let tr  = fromList [(1, "a"), (2, "b")] :: m Int String
                    tr' = delete 3 tr in do {
                    size tr'         @?= 2;
                    Map.lookup 1 tr' @?= Just "a";
                    Map.lookup 2 tr' @?= Just "b"
                }
        ],
        
        testGroup "adjust" [
            testCase "Existing element" $
                let tr  = fromList [(1, 10), (2, 9)] :: m Int Int
                    tr' = adjust (2 ^) 2 tr in do {
                    size tr'         @?= 2;
                    Map.lookup 1 tr' @?= Just 10;
                    Map.lookup 2 tr' @?= Just 512
                },

            testCase "Absent element" $
                let tr  = fromList [(1, 10), (2, 9)] :: m Int Int
                    tr' = adjust (2 ^) 3 tr in do {
                    size tr'         @?= 2;
                    Map.lookup 1 tr' @?= Just 10;
                    Map.lookup 2 tr' @?= Just 9
                }
        ],
        
        testGroup "adjustWithKey" [
            testCase "Existing element" $
                let tr  = fromList [("a", "xy"), ("b", "cd")] :: m String String
                    tr' = adjustWithKey (\k v -> k ++ " " ++ v) "a" tr in do {
                    size tr'           @?= 2;
                    Map.lookup "a" tr' @?= Just "a xy";
                    Map.lookup "b" tr' @?= Just "cd"
                },

            testCase "Absent element" $
                let tr  = fromList [("a", "xy"), ("b", "cd")] :: m String String
                    tr' = adjustWithKey (\ k v -> k ++ " " ++ v) "c" tr in do {
                    size tr'           @?= 2;
                    Map.lookup "a" tr' @?= Just "xy";
                    Map.lookup "b" tr' @?= Just "cd"
                }
        ],
        
        testGroup "update" [
            testCase "Changing existing element" $
                let tr  = fromList [("a", "xy"), ("b", "cd")] :: m String String
                    tr' = update (\v -> Just (v ++ " " ++ v)) "a" tr in do {
                    size tr'           @?= 2;
                    Map.lookup "a" tr' @?= Just "xy xy";
                    Map.lookup "b" tr' @?= Just "cd"
                },

            testCase "Deleting existing element" $
                let tr  = fromList [("a", "xy"), ("b", "cd")] :: m String String
                    tr' = update (\v -> Nothing) "a" tr in do {
                    size tr'           @?= 1;
                    Map.lookup "b" tr' @?= Just "cd"
                },
                
            testCase "Absent element" $
                let tr  = fromList [("a", "xy"), ("b", "cd")] :: m String String
                    tr' = update (\v -> Just (v ++ " " ++ v)) "c" tr in do {
                    size tr'           @?= 2;
                    Map.lookup "a" tr' @?= Just "xy";
                    Map.lookup "b" tr' @?= Just "cd"
                }
        ],
        
        testGroup "updateWithKey" [
            testCase "Changing existing element" $
                let tr  = fromList [("a", "xy"), ("b", "cd")] :: m String String
                    tr' = updateWithKey (\k v -> Just (k ++ ":" ++ v)) "a" tr in do {
                    size tr'           @?= 2;
                    Map.lookup "a" tr' @?= Just "a:xy";
                    Map.lookup "b" tr' @?= Just "cd"
                },

            testCase "Deleting existing element" $
                let tr  = fromList [("a", "xy"), ("b", "cd")] :: m String String
                    tr' = updateWithKey (\k v -> Nothing) "a" tr in do {
                    size tr'           @?= 1;
                    Map.lookup "b" tr' @?= Just "cd"
                },
                
            testCase "Absent element" $
                let tr  = fromList [("a", "xy"), ("b", "cd")] :: m String String
                    tr' = updateWithKey (\k v -> Just (k ++ ":" ++ v)) "c" tr in do {
                    size tr'           @?= 2;
                    Map.lookup "a" tr' @?= Just "xy";
                    Map.lookup "b" tr' @?= Just "cd"
                }
        ],
        
        testGroup "alter" [
            testCase "Changing existing element" $
                let tr  = fromList [(0, 18), (1, 29)] :: m Int Int
                    tr' = alter (\(Just v) -> Just (v + 10)) 0 tr in do {
                    size tr'         @?= 2;
                    Map.lookup 0 tr' @?= Just 28;
                    Map.lookup 1 tr' @?= Just 29
                },

            testCase "Deleting existing element" $
                let tr  = fromList [(0, 18), (1, 29)] :: m Int Int
                    tr' = alter (\(Just k) -> Nothing) 1 tr in do {
                    size tr'         @?= 1;
                    Map.lookup 0 tr' @?= Just 18
                },
            
            testCase "Adding new element" $
                let tr  = fromList [(0, 18), (1, 29)] :: m Int Int
                    tr' = alter (\Nothing -> Just 3) 2 tr in do {
                    size tr'         @?= 3;
                    Map.lookup 0 tr' @?= Just 18;
                    Map.lookup 1 tr' @?= Just 29;
                    Map.lookup 2 tr' @?= Just 3
                },
            
            testCase "Not adding absent element" $
                let tr  = fromList [(0, 18), (1, 29)] :: m Int Int
                    tr' = alter (\Nothing -> Nothing) 2 tr in do {
                    size tr'           @?= 2;
                    Map.lookup 0 tr' @?= Just 18;
                    Map.lookup 1 tr' @?= Just 29
                }
        ],
        
        testGroup "lookup" [
            testCase "Existing element" $
                let tr  = fromList [(1, 3), (2, 4)] :: m Int Int in
                Map.lookup 1 tr @?= Just 3,

            testCase "Absent element" $
                let tr  = fromList [(1, 3), (2, 4)] :: m Int Int in
                Map.lookup 8 tr @?= Nothing,
            
            testCase "Empty map" $
                let tr  = fromList [] :: m Int Int in
                Map.lookup 1 tr @?= Nothing
        ],
        
        testGroup "member and notMember" [
            testCase "Existing element" $
                let tr  = fromList [("a", 3), ("b", 4)] :: m String Int in do {
                    member "a" tr    @?= True;
                    notMember "a" tr @?= False;
                },

            testCase "Absent element" $
                let tr  = fromList [("a", 3), ("b", 4)] :: m String Int in do {
                    member "x" tr    @?= False;
                    notMember "x" tr @?= True;
                }
        ],
        
        testGroup "size" [
            testCase "1-element map" $
                let tr  = fromList [("a", 3)] :: m String Int in
                    size tr @?= 1,

            testCase "5-element map" $
                let tr  = fromList [(1, 2), (2, 3), (3, 4), (4, 5), (5, 6)] :: m Int Int in
                    size tr @?= 5,
            
            testCase "Empty map" $
                let tr  = fromList [] :: m Int Int in
                    size tr @?= 0
        ]
    ]

testNaiveTree :: TestTree
testNaiveTree = testGroup "Test NaiveTree" [
        testGroup "merge" [
            testCase "merge empty" $
                merge Nil Nil @?= (Nil :: NaiveTree () ())
            ,
            testCase "merge two nodes" $
                -- Ваша реализация может выдавать другое дерево, соответствующее
                -- последовательности 1, 2.
                merge (Node 1 "a" Nil Nil) (Node 2 "b" Nil Nil)
                    @?= Node 2 "b" (Node 1 "a" Nil Nil) Nil
        ]
    ]

testMap :: TestTree
testMap = testGroup "Testing implementations of trees"
    [
        mapTests "Data.Map.Strict" (Proxy :: Proxy SMap.Map),
        mapTests "NaiveList" (Proxy :: Proxy NaiveList),
        mapTests "NaiveTree" (Proxy :: Proxy NaiveTree),
        testNaiveTree
    ]
