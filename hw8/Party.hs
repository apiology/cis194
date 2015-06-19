{-# OPTIONS_GHC -fno-warn-orphans #-}

module Party where

import Employee
import Data.Monoid
import Data.Tree
import Data.List



-- glCons should simply add the new Employee and add their fun score
-- without doing any kind of checks. - assumes employee is not already
-- on list
glCons :: Employee -> GuestList -> GuestList
glCons e (GL guests glfun) = GL (e:guests) (glfun + empFun e)

-- Monoid GuestList combines two guestlists on the basis of which one is better.
instance Monoid GuestList where
  mempty  = GL [] 0
  mappend = max

moreFun :: GuestList -> GuestList -> GuestList
moreFun = mappend

treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f (Node a subF) = f a childResults
  where childResults = map (treeFold f) subF
                               
-- combineGLs :: Employee -> [GuestList] -> GuestList

-- The first argument is the “boss” of the current subtree (let’s call
-- him Bob). The second argument is a list of the results for each
-- subtree under Bob. Each result is a pair of GuestLists: the first
-- GuestList in the pair is the best possible guest list with the boss
-- of that subtree; the second is the best possible guest list without
-- the boss of that subtree. nextLevel should then compute the overall
-- best guest list that includes Bob, and the overall best guest list
-- that doesn’t include Bob.

-- returns (best with boss, best without boss)
nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel bob = foldl' add (glCons bob mempty, mempty)
  where add (withBob, withoutBob) (withSubBoss, withoutSubBoss) =
          (glCons bob withoutSubBoss <> withBob,
           withSubBoss <> withoutSubBoss <> withoutBob)

maxFun :: Tree Employee -> GuestList
maxFun company = uncurry (<>) $ treeFold nextLevel company

readEmployeeTree :: String -> Tree Employee
readEmployeeTree = read

formatGuestList :: GuestList -> String
formatGuestList (GL employees fun) =
  "Total fun: " ++ show fun ++ "\n" ++ unlines (map empName employees)

process :: Tree Employee -> String
process = formatGuestList . maxFun

main :: IO ()
main = readFile "company.txt" >>= putStrLn . process . readEmployeeTree
