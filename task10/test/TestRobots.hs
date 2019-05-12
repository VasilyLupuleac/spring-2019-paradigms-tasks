import Test.Tasty
import Test.Tasty.HUnit

import Robots

main :: IO ()
main = defaultMain testsRobots

testsRobots :: TestTree
testsRobots = let
        walter = robot "Walter" 50 50
        bender = robot "Bender" 80 20
        tanky  = robot "Bastion" 22 200
        dead   = robot "T-800" 40 (-25)
    in testGroup "Unit tests for Robots task"
        [ testCase "Test for getName" $
            getName walter @?= "Walter"

        , testCase "Test for getAttack" $
            getAttack bender @?= 80

        , testCase "Test for getHealth" $
            getHealth bender @?= 20

        , testCase "Test for printRobot" $
            printRobot walter @?= "Walter, attack: 50, health: 50"

        , testCase "Test for setName" $
            setName "Marvin" walter @?= robot "Marvin" 50 50

        , testCase "Test for setAttack" $
            setAttack 90 bender @?= robot "Bender" 90 20

        , testCase "Test for setHealth" $
            setHealth 0 walter @?= robot "Walter" 50 0
        
        , testCase "Test for damage" $
            damage bender 30 @?= robot "Bender" 80 (-10)

        , testCase "Test for isAlive on living robot" $
            isAlive walter @?= True

        , testCase "Test for isAlive on dead robot" $
            isAlive dead @?= False

        , testCase "Test for fight with living attacker" $
            fight walter bender @?= robot "Bender" 80 (-30)

        , testCase "Test for fight with dead attacker" $
            fight dead walter @?= walter

        , testCase "Test for threeRoundFight finished after one round" $
            threeRoundFight bender walter @?= bender

		, testCase "Test for threeRoundFight finished after two rounds" $
            threeRoundFight bender tanky @?= robot "Bastion" 22 120

        , testCase "Test for threeRoundFight finished after three rounds" $
            threeRoundFight walter tanky @?= robot "Bastion" 22 100

        , testCase "Test for neueRobotAttak" $
            neueRobotAttak bender @?= robot "Bender" 80 (-30)

        , testCase "Test for survivors" $
		    survivors @?= [robot "David" 50 60]
        ]
