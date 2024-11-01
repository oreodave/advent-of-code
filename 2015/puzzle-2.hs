import qualified Data.Text as DT
import qualified Data.List as DL

parse_line s = map read_int members
  where
    delim = DT.pack "x"
    content = DT.pack s
    members = map DT.unpack $ DT.splitOn delim content
    read_int = \x -> (read x) :: Int

surface_area [l, w, h] = (2 * l * w) + (2 * w * h) + (2 * l * h)
extra_area xs = x * y
  where [x, y] = take 2 $ DL.sort xs
total_area xs = (surface_area xs) + (extra_area xs)

volume [l, w, h] = l * w * h
ribbon_wrap xs = (2 * x) + (2 * y)
  where [x, y] = take 2 $ DL.sort xs

total_ribbon xs = (volume xs) + (ribbon_wrap xs)

main = do
  content <- readFile "2-input"
  let dimensions = map parse_line $ lines content
  let areas = sum $ map total_area dimensions
  let ribbon = sum $ map total_ribbon dimensions
  putStrLn $ "Round 1: " ++ (show areas)
  putStrLn $ "Round 2: " ++ (show ribbon)

-- Local Variables:
-- compile-command: "ghc puzzle-2.hs && ./puzzle-2"
-- End:
