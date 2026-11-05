-- ============================================================
--  L-systems and turtle graphics
--
--  Load with:   ghci LSystem.hs
--  Then run     checks     to test what you have written so far.
--  To draw:     runghc Main.hs      (see Main.hs for options)
--
--  Do not change the types or the names of the functions below:
--  we test your code against them.  You may add as many helper
--  functions as you like.
--
--  Group members (one row each):
--
--    | Name                     | Student ID |
--    |--------------------------|------------|
--    | Robert Hill              | 2968106    |
--    |                          |            |
--    |                          |            |
-- ============================================================

module LSystem where

import Control.Exception (SomeException, evaluate, try)
import Data.List (isInfixOf)
import Text.Printf (printf)

-- ------------------------------------------------------------
-- Part 1: Rewriting
-- ------------------------------------------------------------

-- A list of rules: the pair (x, ys) means "replace x by ys".
-- Symbols with no rule are left alone.

type Rules a = [(a, [a])]

-- An L-system with its drawing information.  (This is "record
-- syntax": it also defines functions  name :: LSystem -> String,
-- axiom :: LSystem -> String  and so on, which you can use to get
-- at the fields.)

data LSystem = LSystem
  { name  :: String      -- used as the output file name
  , axiom :: String      -- the starting string
  , rules :: Rules Char  -- the rewriting rules
  , angle :: Double      -- turning angle, in degrees
  , depth :: Int         -- how many generations look good
  }

-- What a single symbol rewrites to.
--   rewrite [('a', "ab")] 'a'  ==  "ab"
--   rewrite [('a', "ab")] 'z'  ==  "z"

rewrite :: Eq a => Rules a -> a -> [a]
rewrite [] y = [y]
rewrite ((x1,x2):xs) y  | x1 == y = x2
                        | otherwise = rewrite xs y

-- One generation: rewrite every symbol, all at the same time.
--   step [('A', "AB"), ('B', "A")] "ABA"  ==  "ABAAB"

step :: Eq a => Rules a -> [a] -> [a]
step _ [] = []
step rules (x:xs) = rewrite rules x ++ step rules xs

-- The (infinite!) list of all generations, starting with the axiom.

generations :: Eq a => Rules a -> [a] -> [[a]]
generations rules axiom = axiom : generations rules (step rules axiom)

-- Generation n of an L-system.  (expand ls 0 is the axiom.)

expand :: LSystem -> Int -> String
expand system 0 = axiom system
expand system gen = step (rules system) (expand system (gen-1))

-- ------------------------------------------------------------
-- Part 2: Turtle programs
-- ----------------------- -------------------------------------

-- A program for the turtle.  A branch is a sub-program: the turtle
-- runs it and then returns to wherever it was before the branch.

data Instr
  = Draw            -- move forward one step, drawing a line
  | Move            -- move forward one step without drawing
  | Turn Double     -- turn anticlockwise by this many degrees
  | Branch [Instr]  -- run a sub-program, then come back
  deriving (Eq, Show)

-- The meaning of a single symbol, given the turning angle a:
--
--   'F', 'G'   Draw
--   'f'        Move
--   '+'        Turn a          (left)
--   '-'        Turn (-a)       (right)
--   '|'        Turn 180
--
-- Every other symbol (apart from brackets, which are dealt with by
-- parseBlock) means nothing to the turtle: return Nothing.

symbol :: Double -> Char -> Maybe Instr
symbol a x  | x == 'F' || x == 'G' = Just Draw
            | x == 'f' = Just Move
            | x == '+' = Just (Turn a)
            | x == '-' = Just (Turn (-a))
            | x == '|' = Just (Turn 180)
            | otherwise = Nothing

-- Read instructions up to the first unmatched ']' (or the end of the
-- string).  Return the instructions read, and what is left of the
-- string AFTER that ']'.  When you meet a '[', call parseBlock
-- recursively to read the contents of the branch.
--
--   parseBlock 90 "F+F]G-X"  ==  ([Draw, Turn 90, Draw], "G-X")

parseBlock :: Double -> String -> ([Instr], String)
parseBlock a [] = ([], "")
parseBlock a (']':xs) = ([], xs)
parseBlock a ('[':xs) = (Branch (fst parse_sub) : fst parse_rest, snd parse_rest)
                      where 
                        parse_sub = parseBlock a xs
                        parse_rest = parseBlock a (snd parse_sub)

parseBlock a (x:xs) = case symbol a x of
                        Just z -> (z : fst parse_xs, snd parse_xs) 
                        Nothing -> parse_xs
                      where 
                        parse_xs = parseBlock a xs

-- Turn a whole string into a turtle program.  (You may assume the
-- brackets in the string are balanced.)
--
--   parse 90 "F[+F]F"  ==  [Draw, Branch [Turn 90, Draw], Draw]

parse :: Double -> String -> [Instr]
parse a inst_string = fst (parseBlock a inst_string)

-- How many lines will this program draw? (Count the Draws, including
-- the ones inside branches.)

countDraws :: [Instr] -> Int
countDraws [] = 0
countDraws (x:xs) = case x of
                      Draw -> 1 + countDraws xs
                      Branch sub -> countDraws sub + countDraws xs
                      otherwise -> countDraws xs

-- How deeply are branches nested?  A program with no branches has
-- nesting 0; "F[+F][-F[F]]F" has nesting 2.

nesting :: [Instr] -> Int
nesting [] = 0
nesting (Branch sub : xs) = max (1 + (nesting sub)) (nesting xs)
nesting (x:xs) = nesting xs

-- Tidy up a program without changing the picture it draws:
--   * adjacent turns are merged into one:  Turn 30, Turn 15  ~>  Turn 45
--   * a turn by 0 degrees is removed
--   * an empty branch is removed
-- Branches should be simplified on the inside too.

simplify :: [Instr] -> [Instr]
simplify inst_list = if inst_list == inst_list_reduced then inst_list else simplify inst_list_reduced
                    where
                      inst_list_reduced = reduce inst_list
                      reduce inst_list = case inst_list of
                        [] -> []
                        (Turn a : (Turn b : ys)) -> reduce (Turn (a+b) : ys)
                        (Turn 0 : xs) -> reduce xs
                        (Branch [] : xs) -> reduce xs
                        (Branch sub : xs) -> Branch (reduce sub) : reduce xs
                        (x:xs) -> x : reduce xs

-- ------------------------------------------------------------
-- Part 3: The turtle
-- ------------------------------------------------------------

type Point = (Double, Double)
type Line  = (Point, Point)     -- from, to

-- A turtle has a position and a heading.  The heading is in degrees,
-- measured anticlockwise from the positive x-axis (so 0 is east and
-- 90 is north).

data Turtle = Turtle Point Double
  deriving (Eq, Show)

-- Every picture starts at the origin, facing north.

start :: Turtle
start = Turtle (0, 0) 90

position :: Turtle -> Point
position (Turtle p _) = p

-- Move one step (of length 1) in the direction of the heading.
-- Remember that cos and sin expect radians.

forward :: Turtle -> Turtle
forward (Turtle (x,y) heading) = Turtle ((x + cos heading_rad), (y + sin heading_rad)) heading
                                where
                                  heading_rad = (heading / 180) * pi

-- Turn anticlockwise by the given number of degrees.

turn :: Double -> Turtle -> Turtle
turn a (Turtle p heading) = Turtle p (heading + a)

-- Run a program: return where the turtle ends up, and every line it
-- drew, in the order it drew them.  A Branch does not move the
-- turtle: afterwards it carries on from where it was before the
-- branch.

run :: Turtle -> [Instr] -> (Turtle, [Line])
run (Turtle (x,y) heading) inst_list = case inst_list of
                                        [] -> (init_turtle, [])
                                        (Draw : xs) -> let run_xs = run moved_turtle xs in (fst run_xs, ((x,y), position moved_turtle) : snd run_xs)
                                        (Move : xs) -> run moved_turtle xs
                                        (Turn a : xs) -> run (turn a init_turtle) xs
                                        (Branch sub : xs) -> let run_xs = run init_turtle xs in (fst run_xs, snd (run init_turtle sub) ++ snd run_xs)
                                      where
                                        init_turtle = Turtle (x,y) heading
                                        moved_turtle = forward init_turtle

-- All the lines drawn by a program, starting from start.

draw :: [Instr] -> [Line]
draw inst_list = snd (run start inst_list)

-- ------------------------------------------------------------
-- Part 4: SVG output
-- ------------------------------------------------------------

-- Show a number with exactly two decimal places.  (Provided.)
--   num 3.14159  ==  "3.14"     num 2  ==  "2.00"

num :: Double -> String
num x = printf "%.2f" (fromIntegral (round (x * 100) :: Integer) / 100 :: Double)

-- The smallest box containing every line, as (bottom left, top right).
-- You may assume the list is not empty.
--
--   bounds [((0,0),(1,2)), ((-1,5),(3,1))]  ==  ((-1,0),(3,5))

bounds :: [Line] -> (Point, Point)
bounds [] = undefined
bounds [((x1,y1),(x2,y2))] = ((min x1 x2, min y1 y2), (max x1 x2, max y1 y2))
bounds (((x1,y1),(x2,y2)):xs) = ((min_x, min_y), (max_x, max_y))
                                where
                                  bounds_xs = bounds xs
                                  min_x = minimum [x1, x2, fst (fst bounds_xs)]
                                  min_y = minimum [y1, y2, snd (fst bounds_xs)]
                                  max_x = maximum [x1, x2, fst (snd bounds_xs)]
                                  max_y = maximum [y1, y2, snd (snd bounds_xs)]

-- SVG's y-axis points DOWN the page, so we flip every point upside
-- down when we write it out:  (x, y)  is written as  "x -y".
--
--   svgPoint (1.5, 2)  ==  "1.50 -2.00"

svgPoint :: Point -> String
svgPoint (x, y) = unwords [num x, num (-y)]

-- The drawing commands for an SVG path.  Each line (p, q) becomes
-- "M p L q" ("move to p, then draw a line to q"), and the commands
-- for the lines are separated by single spaces:
--
--   svgPath [((0,0),(1,0)), ((1,0),(1,1))]
--     ==  "M 0.00 0.00 L 1.00 0.00 M 1.00 0.00 L 1.00 -1.00"

svgPath :: [Line] -> String
svgPath [] = ""
svgPath [(p1,p2)] = unwords ["M", svgPoint p1, "L", svgPoint p2]
svgPath ((p1,p2):xs) = unwords ["M", svgPoint p1, "L", svgPoint p2, svgPath xs]

-- A complete SVG file.  It should look like this, with one path
-- containing all the lines (how you lay out the spaces and line
-- breaks between attributes is up to you):
--
--   <svg xmlns="http://www.w3.org/2000/svg" width="800" height="800"
--        viewBox="X Y W H">
--     <path fill="none" stroke="black" stroke-width="1.5"
--           vector-effect="non-scaling-stroke"
--           d="PATH"/>
--   </svg>
--
-- where PATH is the result of svgPath, and the viewBox "X Y W H" is
-- the box to show: its top-left corner (X, Y) and its width and
-- height (W, H), all in SVG coordinates -- so mind the flip!  Work
-- it out from bounds, and leave a margin of  pad  on every side, where
--
--   pad = max 1 (0.05 * max width height)

toSVG :: [Line] -> String
toSVG line_list = concat ["<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"800\" height=\"800\" viewBox=\"", unwords [xy, w, h], "\"><path fill=\"none\" stroke=\"black\" stroke-width=\"1.5\" vector-effect=\"non-scaling-stroke\" d=\"", path, "\"/></svg>"]
                where
                  xy = svgPoint ((fst (fst bounds_coor) - pad, snd (snd bounds_coor) + pad))
                  w_nopad = fst (snd bounds_coor) - fst (fst bounds_coor)
                  h_nopad = snd (snd bounds_coor) - snd (fst bounds_coor)
                  w = num (w_nopad + 2*pad)
                  h = num (h_nopad + 2*pad)
                  pad = max 1 (0.05 * max w_nopad h_nopad)

                  path = svgPath line_list
                  bounds_coor = bounds line_list

-- Put it all together: the SVG picture of generation n.

render :: LSystem -> Int -> String
render system gen = toSVG $ draw $ simplify $ parse (angle system) $ expand system gen

-- ------------------------------------------------------------
-- The gallery (provided)
-- ------------------------------------------------------------

algae :: LSystem
algae = LSystem "algae" "A" [('A', "AB"), ('B', "A")] 0 5

koch :: LSystem
koch = LSystem "koch" "-F" [('F', "F+F-F-F+F")] 90 4

snowflake :: LSystem
snowflake = LSystem "snowflake" "F--F--F" [('F', "F+F--F+F")] 60 4

sierpinski :: LSystem
sierpinski = LSystem "sierpinski" "F" [('F', "G-F-G"), ('G', "F+G+F")] 60 7

dragon :: LSystem
dragon = LSystem "dragon" "FX" [('X', "X+YF+"), ('Y', "-FX-Y")] 90 12

hilbert :: LSystem
hilbert = LSystem "hilbert" "A" [('A', "+BF-AFA-FB+"), ('B', "-AF+BFB+FA-")] 90 5

plant :: LSystem
plant = LSystem "plant" "X" [('X', "F-[[X]+X]+F[+FX]-X"), ('F', "FF")] 22.5 6

bush :: LSystem
bush = LSystem "bush" "F" [('F', "FF-[-F+F+F]+[+F-F-F]")] 22.5 4

gallery :: [LSystem]
gallery = [koch, snowflake, sierpinski, dragon, hilbert, plant, bush]

-- ------------------------------------------------------------
-- Self-test (provided)
--
-- Passing these does not guarantee your code is correct, but
-- failing them means it certainly is not!
-- ------------------------------------------------------------

approx :: Point -> Point -> Bool
approx (a, b) (c, d) = abs (a - c) < 1e-9 && abs (b - d) < 1e-9

approxLines :: [Line] -> [Line] -> Bool
approxLines ls ms =
  length ls == length ms && and [approx p p' && approx q q' | ((p, q), (p', q')) <- zip ls ms]

checks :: IO ()
checks = mapM_ check
  [ ("rewrite",     rewrite [('a', "ab")] 'a' == "ab" && rewrite [('a', "ab")] 'z' == "z")
  , ("step",        step (rules algae) "ABAAB" == "ABAABABA")
  , ("step (poly)", step [(1, [1, 2]), (2, [])] [1, 2, 1 :: Int] == [1, 2, 1, 2])
  , ("generations", map length (take 8 (generations (rules algae) "A")) == [1, 2, 3, 5, 8, 13, 21, 34])
  , ("expand",      expand koch 1 == "-F+F-F-F+F" && length (expand dragon 3) == 30)
  , ("symbol",      symbol 60 '-' == Just (Turn (-60)) && symbol 60 'X' == Nothing)
  , ("parseBlock",  parseBlock 90 "F+F]G-X" == ([Draw, Turn 90, Draw], "G-X")
                      && parseBlock 90 "F[F]G]X" == ([Draw, Branch [Draw], Draw], "X"))
  , ("parse",       parse 90 "F[+F][-F[F]]F" ==
                      [Draw, Branch [Turn 90, Draw], Branch [Turn (-90), Draw, Branch [Draw]], Draw]
                      && parse 90 "[F[F]+F]-F" ==
                      [Branch [Draw, Branch [Draw], Turn 90, Draw], Turn (-90), Draw])
  , ("countDraws",  countDraws (parse 90 "F[+F][-F[F]]FX") == 5)
  , ("nesting",     nesting (parse 90 "F[+F][-F[F]]F") == 2 && nesting [Draw] == 0
                      && nesting [] == 0 && nesting [Branch []] == 1)
  , ("simplify",    simplify (parse 90 "F+-+[X][+F-]+F")
                      == [Draw, Turn 90, Branch [Turn 90, Draw, Turn (-90)], Turn 90, Draw]
                      && simplify (parse 90 "F+[]-F") == [Draw, Draw])
  , ("forward",     approx (position (forward start)) (0, 1))
  , ("turn",        turn 45 (Turtle (1, 2) 90) == Turtle (1, 2) 135)
  , ("run",         approxLines (snd (run start [Draw, Turn (-90), Draw, Draw]))
                      [((0, 0), (0, 1)), ((0, 1), (1, 1)), ((1, 1), (2, 1))])
  , ("run (branch)", approxLines (snd (run start [Draw, Branch [Turn (-90), Draw], Turn 90, Draw]))
                      [((0, 0), (0, 1)), ((0, 1), (1, 1)), ((0, 1), (-1, 1))]
                      && approx (position (fst (run start [Draw, Branch [Turn (-90), Draw], Turn 90, Draw]))) (-1, 1))
  , ("draw",        approxLines (draw [Turn (-90), Draw, Move, Draw]) [((0, 0), (1, 0)), ((2, 0), (3, 0))])
  , ("bounds",      bounds [((0, 0), (1, 2)), ((-1, 5), (3, 1))] == ((-1, 0), (3, 5)))
  , ("svgPoint",    svgPoint (1.5, 2) == "1.50 -2.00")
  , ("svgPath",     svgPath [((0, 0), (1, 0)), ((1, 0), (1, 1))]
                      == "M 0.00 0.00 L 1.00 0.00 M 1.00 0.00 L 1.00 -1.00")
  , ("toSVG",       "viewBox=\"-1.00 -2.00 2.00 3.00\"" `isInfixOf` toSVG [((0, 0), (0, 1))]
                      && "viewBox=\"-2.00 -42.00 14.00 44.00\"" `isInfixOf` toSVG [((0, 0), (0, 40)), ((0, 40), (10, 30))])
  ]
  where
    check (n, ok) = do
      r <- try (evaluate ok) :: IO (Either SomeException Bool)
      putStrLn (n ++ ": " ++ case r of
                               Right True  -> "ok"
                               Right False -> "FAILED"
                               Left _      -> "not done yet (or crashed)")
