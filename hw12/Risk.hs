{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

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

numAttackerDiceRolls :: Battlefield -> Army
numAttackerDiceRolls battlefield = min 3 $ attackers battlefield - 1
numDefenderDiceRolls :: Battlefield -> Army
numDefenderDiceRolls battlefield = min 2 $ defenders battlefield

sortDesc :: Ord a => [a] -> [a]
sortDesc = sortBy (flip compare)

attackerRolls :: Battlefield -> Rand StdGen [DieValue]
attackerRolls battlefield = liftM sortDesc $ replicateM (numAttackerDiceRolls battlefield) die

defenderRolls :: Battlefield -> Rand StdGen [DieValue]
defenderRolls battlefield = liftM sortDesc $ replicateM (numDefenderDiceRolls battlefield) die

rollResults :: Battlefield -> Rand StdGen [Bool]
rollResults battlefield = liftM2 (zipWith (>)) aRolls dRolls
  where aRolls = attackerRolls battlefield
        dRolls = defenderRolls battlefield
                   
testBf :: Battlefield
testBf = Battlefield 10 2

-- "should simulate randomly rolling the appropriate number of dice,
-- interpreting the results, and updating the two armies to reflect
-- casualties. You may assume that each player will attack or defend
-- with the maximum number of units they are allowed."
battle :: Battlefield -> Rand StdGen Battlefield
battle battlefield = liftM2 Battlefield newAttackerStrength newDefenderStrength
    where newAttackerStrength = attackerLosses >>= (\a -> return (attackers battlefield - a))
          newDefenderStrength = defenderLosses >>= (\a -> return (defenders battlefield - a))
          attackerLosses = liftM (length . filter (==True)) roll
          defenderLosses = liftM (length . filter (==False)) roll
          roll = rollResults battlefield

-- XXX: Go back and make the above prettier

