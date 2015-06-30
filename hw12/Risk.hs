{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad.Loops
import Control.Monad.Random
import Control.Monad
import Data.List

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int } 
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }
  deriving (Show)

sortDesc :: Ord a => [a] -> [a]
sortDesc = sortBy (flip compare)

testBf :: Battlefield
testBf = Battlefield 10 2

-- "should simulate randomly rolling the appropriate number of dice,
-- interpreting the results, and updating the two armies to reflect
-- casualties. You may assume that each player will attack or defend
-- with the maximum number of units they are allowed."
battle :: Battlefield -> Rand StdGen Battlefield
battle battlefield = do
  let numDRolls = min 2 $ defenders battlefield
  rawDRolls <- replicateM numDRolls die
  let dRolls = sortDesc rawDRolls
      numARolls = min 3 $ attackers battlefield - 1
  rawARolls <- replicateM numARolls die
  let aRolls = sortDesc rawARolls
  let roll = zipWith (>) aRolls dRolls
      attackerLosses = length $ filter (==True) roll
      newAttackerStrength = attackers battlefield - attackerLosses
      defenderLosses = length roll - attackerLosses
      newDefenderStrength = defenders battlefield - defenderLosses
  return $ Battlefield newAttackerStrength newDefenderStrength

canBattle :: Battlefield -> Bool
canBattle battlefield = (attackers battlefield >= 2) && (defenders battlefield > 0)

invade :: Battlefield -> Rand StdGen Battlefield
invade battlefield =
  if canBattle battlefield
  then
     do
       newBattlefield <- battle battlefield
       invade newBattlefield
   else
     return battlefield

attackerWon :: Battlefield -> Bool
attackerWon battlefield = defenders battlefield == 0

characterize :: [Battlefield] -> Double
characterize endgames = numAttackerWins / numEndgames
  where numEndgames = fromIntegral $ length endgames
        numAttackerWins = fromIntegral $ length (filter attackerWon endgames)

successProb :: Battlefield -> Rand StdGen Double
successProb battlefield = do
  invasions <- replicateM 1000 $ invade battlefield
  return $ characterize invasions
