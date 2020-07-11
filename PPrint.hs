module PPrint where
import Data.Foldable

writeln :: String -> IO ()
writeln = putStrLn

showsPair :: Show a => (String, a) -> ShowS
showsPair (k,v) = showString k . showString ": " . shows v

pprH, pprV :: [ShowS] -> ShowS
pprV list = intercalateS (showString "\n") list
pprH list = intercalateS (showString " ") list

intercalateS :: ShowS -> [ShowS] -> ShowS -- https://stackoverflow.com/a/42360812
intercalateS sep []    = showString ""
intercalateS sep (x:y) = foldl' (\a b -> a . sep . b) x y

pprListWith :: (a -> ShowS) -> [a] -> ShowS
pprListWith f list = pprV (map f list)

runShows :: ShowS -> IO ()
runShows = putStrLn . ($"")
