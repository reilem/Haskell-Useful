data Turtle = Stand | Turn Double Turtle | Move Double Turtle
  deriving(Show)

done :: Turtle
done = Stand

turn :: Double -> Turtle
turn a = Turn a done

step :: Double -> Turtle
step d = Move d done

(>>>) :: Turtle -> Turtle -> Turtle
Turn a p >>> t = Turn a (p >>> t)
Move d p >>> t = Move d (p >>> t)
Stand    >>> t = t

square :: Turtle
square = foldl (>>>) done (replicate 4 (step 50 >>> turn 90))

type Point = (Double, Double)
type Line = (Point, Point)

turtleToLines :: Turtle -> [Line]
turtleToLines turtle =
  plotTurtle turtle 0 (1000,100)
  where
    plotTurtle :: Turtle -> Double -> Point -> [Line]
    plotTurtle Stand d s = []
    plotTurtle (Move l t) d start@(x,y) =
      let nextX = x + l * sin(d * ((2 * pi) / 360)) in
      let nextY = y + l * cos(d * ((2 * pi) / 360)) in
      let end = (nextX,nextY) in
      ((start,end):plotTurtle t d end)
    plotTurtle (Turn a t) d start =
      let nextD = d + a in
      plotTurtle t nextD start

linesToSVG :: [Line] -> String
linesToSVG lines =
  let header = "<svg xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\">" in
  let body = convertLines lines in
  let footer = "</svg>\n" in
  header ++ "\n" ++ body ++ footer
  where
    convertLines :: [Line] -> String
    convertLines [] = ""
    convertLines (((x1,y1),(x2,y2)):ls) =
      "<line x1=\"" ++ show x1 ++ "\" y1=\"" ++ show y1 ++ "\" x2=\"" ++ show x2
      ++ "\" y2=\"" ++ show y2 ++ "\" stroke=\"blue\" stroke-width=\"4\" />\n"
      ++ convertLines ls

-- "/Users/Reinert/Desktop/MySvg.svg"
writeSVG :: FilePath -> Turtle -> IO ()
writeSVG path turtle = writeFile path $ linesToSVG $ turtleToLines turtle

data Fractal = Done | Twist Double Fractal | Step Fractal
  deriving(Show)

fdone :: Fractal
fdone = Done

fturn :: Double -> Fractal
fturn d = Twist d fdone

fstep :: Fractal
fstep = Step fdone

(>->) :: Fractal -> Fractal -> Fractal
Twist d f1 >-> f2  = Twist d (f1 >-> f2)
Step  f1   >-> f2  = Step (f1 >-> f2)
Done       >-> f   = f

concretize :: Double -> Fractal -> Turtle
concretize d (Twist a f) = Turn a $ concretize d f
concretize d (Step f)    = Move d $ concretize d f
concretize d (Done)      = Stand

refine :: Fractal -> Fractal -> Fractal
refine expan (Step f)     = expan >-> refine expan f
refine expan (Twist a f)  = Twist a (refine expan f)
refine expan (Done)       = Done

times :: Int -> (a -> a) -> (a -> a)
times n f
  | n > 0     = f . times (n-1) f
  | otherwise = id

exam :: Fractal -> Fractal -> Int -> Double -> FilePath -> IO ()
exam prog expan n size path =
  writeSVG path $ concretize size $ (times n $ refine expan) prog

expansion =
  fstep >-> fturn (60)   >->
  fstep >-> fturn (-120) >->
  fstep >-> fturn (60)   >->
  fstep

program =
  fstep >-> fturn (-120) >->
  fstep >-> fturn (-120) >->
  fstep

































--
