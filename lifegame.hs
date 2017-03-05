import Control.Concurrent (threadDelay)
import Data.List
import System.Environment
import System.Exit
import System.IO

main :: IO ()
main = do
    [file] <- getArgs
    withFile file ReadMode $ \handle -> do
        field <- map2 (== 'x') . lines <$> hGetContents handle
        putFieldAndTick field []
    where
        putFieldAndTick field prevFields = do
            putField $! field
            putStrLn ""
            case elemIndex field prevFields of
                 Just i -> do
                     putStr $ "Loop found: Start = " ++ (show $ (length prevFields) - i)
                     putStrLn $ " Duration = " ++ (show (i + 1))
                     exitSuccess
                 Nothing -> do
                     threadDelay 100000
                     putFieldAndTick (updateField field) (field:prevFields)


type Cell = Bool
type Field = [[Cell]]

updateField :: Field -> Field
updateField field = field'
    where
        getCell (x, y) = field !! y !! x
        getAround = borderCheck (length (head field), length field)
        getAroundAliveCount coord = length . filter id . map getCell $ getAround coord
        field' = map2 checkCell $ withCoordinate field
        whenAlive coord = count > 1 && count < 4
            where count = getAroundAliveCount coord
        whenDead coord = getAroundAliveCount coord == 3
        checkCell (cell, coord) = if cell
                                     then whenAlive coord
                                     else whenDead coord


addTuple :: (Int, Int) -> (Int, Int) -> (Int, Int)
addTuple (a, b) (c, d) = (a + c, b + d)


borderCheck :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
borderCheck (width, height) coord = coords
    where
        checker = baseChecker (width, height)
        coords = map (addTuple coord . fst) $ filter (\(d, f) -> f (addTuple d coord)) checker


baseChecker :: (Int, Int) -> [((Int, Int), (Int, Int) -> Bool)]
baseChecker (width, height) = [ ((1, 0), \(x, _) -> x < width)
                              , ((1, 1), \(x, y) -> x < width && y < height)
                              , ((0, 1), \(_, y) -> y < height)
                              , ((-1, 1), \(x, y) -> x >= 0 && y < height)
                              , ((-1, 0), \(x, _) -> x >= 0)
                              , ((-1, -1), \(x, y) -> x >= 0 && y >= 0)
                              , ((0, -1), \(_, y) -> y >= 0)
                              , ((1, -1), \(x, y) -> x < width && y >= 0)
                              ]


map2 :: (a -> b) -> [[a]] -> [[b]]
map2 = map . map


withCoordinate :: [[a]] -> [[(a, (Int, Int))]]
withCoordinate field = field'
    where
        h = length field
        w = length $ head field
        withY = zip [0..h] field
        field' = map (\(y, row) -> map (\(x, cell) -> (cell, (x, y))) (zip [0..w] row)) withY


putField :: Field -> IO ()
putField = putStrLn . unlines . map2 showCell
    where
        showCell cell = if cell then 'x' else '_'
