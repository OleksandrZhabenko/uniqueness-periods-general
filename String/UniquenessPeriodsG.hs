-- |
-- Module      :  String.UniquenessPeriodsG
-- Copyright   :  (c) OleksandrZhabenko 2020
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  olexandr543@yahoo.com
--
-- Can be used to produce the similar to 'String.Ukrainian.UniquenessPeriods' from 
-- @uniqueness-periods@ package functions. Provides the generalization of them. 
-- For all the used conversion functions of the type @g :: String -> Vector String@ 
-- it is important that they are stable for the repeated application (their result after 
-- the first application cannot be changed by the rules in the function into new variants). 
-- Otherwise, the recursive scheme of the functions in the module will lead to wrong results.
-- So the conversion function should work the following way (@xs@ denotes a word in the language) in GHCi: 
--
-- > let v = g xs
-- > let ys = concat . toList $ v 
-- > let v2 = g ys
-- > v == v2
-- > True
-- 
-- Or in the other words, for the single word, @g . concat . toList . g = g@;
-- 

module String.UniquenessPeriodsG (
  -- * Auxiliary functions
  show7s'''
  , show7s5
  , show7s6
  , show7sn4'
  , show7snc
  -- ** Inner predicate (auxiliary)
  , eqSnds
  -- ** Inner backward conversion function
  , listToString
  -- * uniquenessPeriods function
  , uniquenessPeriods
) where

import Data.Char (isSpace)
import qualified Data.Vector as V
import Data.List ((\\),nubBy)

-- | Function 'listToString' converts the list of Strings being the sequential sounds representations into the text with whitespaces
-- (whitespaces are substituted instead of punctuation symbols, too) and some phonetic conversions. The first argument must be a list of 'String', 
-- each of which is a representation for the white space (or more generally, non-sound symbol representation).
listToString :: [String] -> [String] -> String
listToString whspss = concatMap (\ts -> if ts `elem` whspss then " " else ts)

-- | Function 'eqSnds' compares two non-silent Strings representations for sounds by equality. If one of them is a representation for silence (e. g. pause),
-- then the predicate is @False@. The first argument must be a list of 'String', 
-- each of which is a representation for the white space (or more generally, non-sound symbol representation).
eqSnds :: [String] -> String -> String -> Bool
eqSnds whspss xs ys 
 | xs `elem` whspss || ys `elem` whspss = False
 | otherwise = xs == ys      

-- | The same as @show7s''@ from MMSyn7s module (@mmsyn7s@ package), but the second element in the resulting tuple is again the text with 
-- whitespaces (whitespaces are substituted
-- instead of punctuation symbols, too) and some phonetic conversions. The first argument must be a list of 'String', 
-- each of which is a representation for the white space (or more generally, non-sound symbol representation).
show7s''' :: [String] -> [String] -> ([String],String)
show7s''' whspss zss =
  let (xss, yss) = splitAt 200 zss
      uss = xss \\ nubBy (eqSnds whspss) xss
      (wss,vss) = if null uss then (xss,[]) else (takeWhile (/= head uss) xss ++ head uss:(takeWhile (/= head uss) . tail . dropWhile (/= head uss) $ xss),
        dropWhile (/= head uss) . tail . dropWhile (/= head uss) $ xss) in 
          (filter (\x -> x `notElem` whspss) $ wss, listToString whspss $ vss ++ yss)

-- | Function 'show7s5' takes a text being a @String@ and returns a tuple, the first element of which is a list of Strings that correspond to the 
-- sounds representations that (except pauses) are unique and are not repeated starting from the beginning of the given text (this list is filtered from 
-- the representations for the silence), and the second one is a @String@ obtained from the remainder
-- list of Strings starting from the first duplicated non-silent sound representation with whitespaces (whitespaces are substituted
-- instead of punctiuation symbols, too) and some phonetic conversions. The first argument must be a list of 'String', 
-- each of which is a representation for the white space (or more generally, non-sound symbol representation). The second argument is a function that 
-- converts a 'String' of the text into the 'V.Vector' of sound representations for that language. 
show7s5 :: [String] -> (String -> V.Vector String) -> String -> ([String], String)
show7s5 whspss g = show7s''' whspss . V.toList . g

-- | Function 'show7s6' takes a text being a @String@ and returns a list of lists of Strings, each latter one of which is obtained for the unique parts of
-- the text from the sounds representations point of view. It can show how many and what sound representations are needed to be created to completely cover
-- the given text providing all the needed sound parameters. The first argument must be a list of 'String', 
-- each of which is a representation for the white space (or more generally, non-sound symbol representation). The second argument is a function that 
-- converts a 'String' of the text into the 'V.Vector' of sound representations for that language. 
show7s6 :: [String] -> (String -> V.Vector String) -> String -> [[String]]
show7s6 whspss g t@(_:_) = (fst . show7s5 whspss g $ t):(show7s6 whspss g . snd . show7s5 whspss g $ t)
show7s6 _ _ _ = []
      
-- | Function 'uniquenessPeriods' takes a text being a @String@ and returns a list of Ints. Each Int value is a number of 
-- the sounds representations (non-silent ones) being unique and not duplicated alongside the given text starting from the beginning to the end.
-- This function provides some important information about the phonetic and in some cases semantic structures of the text. 
-- The first argument must be a list of 'String', 
-- each of which is a representation for the white space (or more generally, non-sound symbol representation). The second argument is a function that 
-- converts a 'String' of the text into the 'V.Vector' of sound representations for that language. 
uniquenessPeriods :: [String] -> (String -> V.Vector String) -> String -> [Int]
uniquenessPeriods whspss g xs 
  | any (not . isSpace) xs = fmap length . show7s6 whspss g $ xs
  | otherwise = [0::Int]

-- | Converts a list of 'String' each one being a non-silent sound representation into a list of 'Int' using recursively @show7sn4'@. 
-- The first argument must be a list of 'String', 
-- each of which is a representation for the white space (or more generally, non-sound symbol representation). 
show7snc :: [String] -> [String] -> [Int]
show7snc whspss xss = let (tss,vss) = show7sn4' whspss xss in if null vss then [length tss] else length tss:show7snc whspss vss
                           
-- | The same as @show7sn'''@ from the MMSyn7s module from the @mmsyn7s@ package, but does not concatenate the list of 'String' as the second tuple's element. 
-- The first argument must be a list of 'String', 
-- each of which is a representation for the white space (or more generally, non-sound symbol representation). 
show7sn4' :: [String] -> [String] -> ([String],[String])
show7sn4' whspss zss =
  let (xss, yss) = splitAt 200 zss
      uss = xss \\ nubBy (eqSnds whspss) xss
      (wss,vss) = if null uss then (xss,[]) else (takeWhile (/= head uss) xss ++ head uss:(takeWhile (/= head uss) . tail . dropWhile (/= head uss) $ xss),
        dropWhile (/= head uss) . tail . dropWhile (/= head uss) $ xss) in 
          (filter (\x -> x `notElem` whspss) $ wss, vss ++ yss)                            
