module Main where

import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Data.Bifunctor
import Data.List
import Data.Maybe
import Data.STRef
import qualified Data.Set as Set
import System.IO
import System.Random

--                 |       |  ### |  ###   |  ###   |   ##  |  ##    | ##
--                 | ####  |    # |  #     |   #    |  ##   |  ##    |  ##
data Block = Blank | Azure | Blue | Orange | Purple | Green | Yellow | Cocoa
  deriving (Eq, Ord)

instance Show Block where
  show Blank = " "
  show Azure = "\ESC[94;104mA\ESC[0m"
  show Blue = "\ESC[34;44mB\ESC[0m"
  show Orange = "\ESC[91;101mO\ESC[0m"
  show Purple = "\ESC[95;105mP\ESC[0m"
  show Green = "\ESC[92;102mG\ESC[0m"
  show Yellow = "\ESC[93;103mY\ESC[0m"
  show Cocoa = "\ESC[31;41mC\ESC[0m"

showRow :: [Block] -> [String]
showRow row = replicate 2 (concatMap show $ concatMap (replicate 4) row)

showBoard :: Board -> String
showBoard = unlines . concatMap showRow

allBlocks :: [Block]
allBlocks = concat $ replicate 2 [Azure, Blue, Orange, Purple, Green, Yellow, Cocoa]

rows :: Int
rows = 8

columns :: Int
columns = 7

type Board = [[Block]]

empty :: Board
empty = replicate rows (replicate columns Blank)

type Mask = [(Int, Int)]

masks :: Block -> [Mask]
masks Blank = []
masks Azure =
  [ [(0, 0), (0, 1), (0, 2), (0, 3)],
    [(0, 0), (1, 0), (2, 0), (3, 0)]
  ]
masks Blue =
  [ [(0, 0), (0, 1), (0, 2), (1, 2)],
    [(0, 0), (1, 0), (2, 0), (2, -1)],
    [(0, 0), (1, 0), (1, 1), (1, 2)],
    [(0, 0), (0, 1), (1, 0), (2, 0)]
  ]
masks Orange =
  [ [(0, 0), (0, 1), (0, 2), (1, 0)],
    [(0, 0), (0, 1), (1, 1), (2, 1)],
    [(0, 0), (1, 0), (1, -1), (1, -2)],
    [(0, 0), (1, 0), (2, 0), (2, 1)]
  ]
masks Purple =
  [ [(0, 0), (0, 1), (0, 2), (1, 1)],
    [(0, 0), (1, 0), (1, -1), (2, 0)],
    [(0, 0), (1, -1), (1, 0), (1, 1)],
    [(0, 0), (1, 0), (2, 0), (1, 1)]
  ]
masks Green =
  [ [(0, 0), (0, 1), (1, -1), (1, 0)],
    [(0, 0), (1, 0), (1, 1), (2, 1)]
  ]
masks Yellow = [[(0, 0), (0, 1), (1, 0), (1, 1)]]
masks Cocoa =
  [ [(0, 0), (0, 1), (1, 1), (1, 2)],
    [(0, 0), (1, 0), (1, -1), (2, -1)]
  ]

usedBlocks :: Board -> [Block]
usedBlocks = concatMap compact . group . sort . filter (/= Blank) . concat
  where
    compact xs = take (length xs `div` 4) xs

nextBlank :: Board -> Maybe (Int, Int)
nextBlank board = listToMaybe [(r, c) | (r, row) <- zip [0 ..] board, (c, x) <- zip [0 ..] row, x == Blank]

putBlock :: (Int, Int) -> Mask -> Block -> Board -> Maybe Board
putBlock (sr, sc) mask block board =
  if isValid
    then Just (chop columns [f r c x | (r, row) <- zip [0 ..] board, (c, x) <- zip [0 ..] row])
    else Nothing
  where
    positions = Set.fromList $ map (bimap (sr +) (sc +)) mask
    isValid = all (\(r, c) -> r >= 0 && r < rows && c >= 0 && c < columns && board !! r !! c == Blank) positions
    f r c x = if (r, c) `Set.member` positions then block else x

chop :: Int -> [a] -> [[a]]
chop n [] = []
chop n xs = take n xs : chop n (drop n xs)

extend :: Board -> [Board]
extend board =
  do
    s <- maybeToList (nextBlank board)
    block <- nub $ allBlocks \\ usedBlocks board
    mask <- masks block
    maybeToList (putBlock s mask block board)

won :: Board -> Bool
won = all (notElem Blank)

findRandomSolution :: StdGen -> Board -> Maybe Board
findRandomSolution gen x
  | won x = Just x
  | otherwise = listToMaybe $ mapMaybe (findRandomSolution gen') x'
  where
    (x', gen') = shuffle' (extend x) gen

-- taken from: https://wiki.haskell.org/Random_shuffle
shuffle' :: [a] -> StdGen -> ([a], StdGen)
shuffle' xs gen =
  runST
    ( do
        g <- newSTRef gen
        let randomRST lohi = do
              (a, s') <- fmap (randomR lohi) (readSTRef g)
              writeSTRef g s'
              return a
        ar <- newArray n xs
        xs' <- forM [1 .. n] $ \i -> do
          j <- randomRST (i, n)
          vi <- readArray ar i
          vj <- readArray ar j
          writeArray ar j vi
          return vj
        gen' <- readSTRef g
        return (xs', gen')
    )
  where
    n = length xs
    newArray :: Int -> [a] -> ST s (STArray s Int a)
    newArray n xs = newListArray (1, n) xs

findSolution :: Board -> Maybe Board
findSolution x
  | won x = Just x
  | otherwise = listToMaybe $ mapMaybe findSolution (extend x)

findSolutions :: Board -> [Board]
findSolutions x
  | won x = [x]
  | otherwise = concatMap findSolutions (extend x)

main :: IO ()
main =
  do
    gen <- getStdGen
    case findRandomSolution gen empty of
      Just b ->
        putStr (showBoard b)
      Nothing ->
        putStr "Not found!"
