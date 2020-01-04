{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Tasty
import Test.Tasty.HUnit as HU
import Controller
import Entity
import Database.Persist.Sql

import Data.List (sort, maximumBy)
import Data.Time
import Data.Time.Calendar (Day (..))
import Data.Time.Clock


hoge :: TestTree
hoge = HU.testCase "fuga" $
    1 + 2 HU.@?= 3


frg :: [TaskFrag]
frg = frg4

frg1 = [
    TaskFrag (2,3) (Just 400) (Just 500) (Just 30)
    , TaskFrag (3,4) (Just 300) (Just 400) (Just 30)
    , TaskFrag (4,5) (Just 200) (Just 300) (Just 30)
    , TaskFrag (5,6) (Just 100) (Just 200) (Just 30)
    ]

frg2 = [
    TaskFrag (2,3) (Nothing) (Nothing) (Just 90)
    , TaskFrag (3,4) (Nothing) (Nothing) (Just 90)
    , TaskFrag (4,5) (Nothing) (Nothing) (Just 90)
    , TaskFrag (5,6) (Just 100) (Nothing) (Just 90)
    ]

frg3 = [
    TaskFrag (1,2) (Nothing) (Just 600) (Just 200)
    , TaskFrag (3,4) (Nothing) (Just 600) (Just 300)
    , TaskFrag (5,6) (Nothing) (Just 600) (Just 100)
    ]

frg4 = [
    TaskFrag (1,2) (Nothing) (Just 600) (Just 200)
    , TaskFrag (3,4) (Nothing) (Just 600) (Just 200)
    , TaskFrag (5,6) (Nothing) (Just 600) (Just 200)
    ]

pianoRollFTest ::  Int -> TestTree
pianoRollFTest reso = HU.testCase "pianoRollF" $
    pianoRollF [(0,1000), (2000,3000)] reso 0 starMan [] HU.@?= []

pianoRollFTest2 :: TestTree
pianoRollFTest2 = HU.testCase "pianoRollF2" $
    pianoRollF (stripedPattern [(0,300)]) 60 0 starMan [] HU.@?= []

winner :: Millis -> [TaskFrag] -> [TaskFrag] -> TaskFrag
winner cursor frags entry =
    maximumBy (\f g -> compare (urgencyF cursor frags f) (urgencyF cursor frags g)) entry

winnerTest :: Millis -> [TaskFrag] -> [TaskFrag] -> TestTree
winnerTest cursor frags entry = HU.testCase "winnerTest" $
    winner cursor frags entry HU.@?= TaskFrag (3,4) (Nothing) (Just 600) (Just 300)

urgencyFTest :: Millis -> [TaskFrag] -> TaskFrag -> TestTree
urgencyFTest cursor frags f = HU.testCase "urgencyFTest" $
    urgencyF cursor frags f HU.@?= Nothing

urgencyByPathFTest :: Millis -> [TaskFrag] -> TestTree
urgencyByPathFTest cursor path = HU.testCase "urgencyPathTest" $
    urgencyByPathF cursor path HU.@?= Nothing

grandDeadlineTest ::  [TaskFrag] -> TestTree
grandDeadlineTest path = HU.testCase "grandDeadlineTest" $
    grandDeadline path HU.@?= Just 1

totalWeightTest :: [TaskFrag] -> TestTree
totalWeightTest path = HU.testCase "totalWeightTest" $
    totalWeight path HU.@?= 0

starMan = [
    TaskFrag (1,2) (Nothing) (Just 1000) (Just 60)
    , TaskFrag (2,3) (Nothing) (Nothing) (Just 60)
    , TaskFrag (2,4) (Nothing) (Nothing) (Just 60)
    , TaskFrag (2,5) (Nothing) (Nothing) (Just 60)
    , TaskFrag (2,6) (Nothing) (Nothing) (Just 60)
    , TaskFrag (3,7) (Nothing) (Nothing) (Just 60)
    , TaskFrag (3,5) (Nothing) (Nothing) (Just 60)
    , TaskFrag (4,8) (Nothing) (Nothing) (Just 60)
    , TaskFrag (5,9) (Nothing) (Nothing) (Just 60)
    , TaskFrag (6,3) (Nothing) (Nothing) (Just 60)
    , TaskFrag (6,4) (Nothing) (Nothing) (Just 60)
    , TaskFrag (6,10) (Nothing) (Nothing) (Just 60)
    ]

footL = TaskFrag (4,8) (Nothing) (Nothing) (Just 60)

routeFromFootL = [
    [
        TaskFrag (4,8) (Nothing) (Nothing) (Just 60)
        , TaskFrag (6,4) (Nothing) (Nothing) (Just 60)
        , TaskFrag (2,6) (Nothing) (Nothing) (Just 60)
        , TaskFrag (1,2) (Nothing) (Just 1000) (Just 60)
    ],
    [
        TaskFrag (4,8) (Nothing) (Nothing) (Just 60)
        , TaskFrag (2,4) (Nothing) (Nothing) (Just 60)
        , TaskFrag (1,2) (Nothing) (Just 1000) (Just 60)
    ]
    ]

routeFindFTest :: TestTree
routeFindFTest = HU.testCase "routeFindFTest" $
    routeFindF starMan footL HU.@?= routeFromFootL

successorTest :: TestTree
successorTest = HU.testCase "successorTest" $
    successor starMan footL HU.@?= []

-- deUser = User {userName = "develop", userAdmin = True, userTimeZone = 9, userIsLazy = False, userResolutionMin = 60, userDefaultDpy = Just 730, userLookUp = Nothing, userLookDown = Nothing, userEncrypted = Nothing}
-- deDurs = [Duration {durationLeft = TimeOfDay 8 30 0, durationRight = TimeOfDay 12 0 0, durationUser = UserKey {unUserKey = SqlBackendKey {unSqlBackendKey = 1}}},Duration {durationLeft = TimeOfDay 13 0 0, durationRight = TimeOfDay 17 30 0, durationUser = UserKey {unUserKey = SqlBackendKey {unSqlBackendKey = 1}}}]
-- deTasks = [Entity {entityKey = TaskKey {unTaskKey = SqlBackendKey {unSqlBackendKey = 4}}, entityVal = Task {taskTerminal = 6, taskInitial = 7, taskIsDummy = False, taskIsDone = False, taskIsStarred = True, taskLink = Nothing, taskStartable = Nothing, taskDeadline = Nothing, taskWeight = Just 10.0, taskTitle = Just "step", taskUser = UserKey {unUserKey = SqlBackendKey {unSqlBackendKey = 1}}}},Entity {entityKey = TaskKey {unTaskKey = SqlBackendKey {unSqlBackendKey = 13}}, entityVal = Task {taskTerminal = 19, taskInitial = 20, taskIsDummy = False, taskIsDone = False, taskIsStarred = False, taskLink = Nothing, taskStartable = Just UTCTime (Day 2020 1 5) (TimeOfDay 15 0 0), taskDeadline = Just UTCTime (Day 2020 1 9) (TimeOfDay 15 0 0), taskWeight = Just 10.0, taskTitle = Just "jump", taskUser = UserKey {unUserKey = SqlBackendKey {unSqlBackendKey = 1}}}},Entity {entityKey = TaskKey {unTaskKey = SqlBackendKey {unSqlBackendKey = 16}}, entityVal = Task {taskTerminal = 23, taskInitial = 24, taskIsDummy = False, taskIsDone = False, taskIsStarred = False, taskLink = Nothing, taskStartable = Nothing, taskDeadline = Just UTCTime (Day 2020 1 9) (TimeOfDay 15 0 0), taskWeight = Just 10.0, taskTitle = Just "jump", taskUser = UserKey {unUserKey = SqlBackendKey {unSqlBackendKey = 1}}}}]
-- deNow = UTCTime (0 :: Day) (0 :: DiffTime)

-- scheduleForwardTest :: TestTree
-- scheduleForwardTest = HU.testCase "scheduleForwardTest" $
--     scheduleForward deUser deNow deDurs deTasks HU.@?= Right []

allTests = testGroup "Tasty Tests" [
    testGroup "HUnit Tests" [
          hoge
        -- , pianoRollFTest2
        , scheduleForwardTest
        -- , winnerTest 0 frg frg
        -- , urgencyFTest 0 frg (TaskFrag (3,4) (Nothing) (Just 600) (Just 300))
        -- , urgencyFTest 0 frg (TaskFrag (5,6) (Just 100) (Just 200) (Just 30))
        -- , routeFindFTest
        ]
    ]

main :: IO ()
main = defaultMain allTests
