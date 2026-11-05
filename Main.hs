-- ============================================================
--  L-systems: driver (provided -- you do not need to edit this)
--
--    runghc Main.hs              draw every picture in the gallery
--    runghc Main.hs plant        draw just one, at its usual depth
--    runghc Main.hs plant 3      draw one at a chosen depth
--
--  Pictures are written as  generated/<name>.svg  in the current directory;
--  open them in any web browser.
-- ============================================================

import LSystem
import System.Environment (getArgs)

save :: LSystem -> Int -> IO ()
save ls n = do
  let file = "generated/" ++ name ls ++ ".svg"
  writeFile file (render ls n)
  putStrLn ("wrote " ++ file ++ " (depth " ++ show n ++ ")")

main :: IO ()
main = do
  args <- getArgs
  case args of
    []          -> mapM_ (\ls -> save ls (depth ls)) gallery
    [s]         -> withSystem s (\ls -> save ls (depth ls))
    [s, n]      -> withSystem s (\ls -> save ls (read n))
    _           -> putStrLn "usage: runghc Main.hs [name [depth]]"
  where
    withSystem s k = case [ls | ls <- gallery, name ls == s] of
      ls : _ -> k ls
      []     -> putStrLn ("no system called " ++ show s ++ "; try one of "
                          ++ unwords (map name gallery))
