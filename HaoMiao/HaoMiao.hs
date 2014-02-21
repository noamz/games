{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

{-
1. There are two players.  Without loss of generality, we will assume they are named Milo and Otis.

2. There are 2*N pieces, where N=8, including N "kittens" and N "puppies". At the beginning of the game, the N kittens are all in Milo's "house", while the N puppies are all in Otis's house.  During successive rounds, some number of kittens and puppies will be brought into "the field", which consists of an (initially empty) collection of (non-empty) piles of animals.

3. The player with the longest tail gets to make the first move. WLOG, we assume that Milo has the longest tail.

4. At each turn, a player has a choice between taking one piece out from that player's house and into the field, or else moving one of the pieces (a kitten or a puppy) already in the field.

4a. When moving one of their pieces into the field, a player has a choice between creating a new pile, or else of placing their piece at the top of an existing pile of animals.

4b. The same applies when moving one of the pieces already in the field, with the proviso that it is illegal to move a piece which was moved in the previous round.

4c. In any case, it is illegal to build a pile of height greater than H, where H=3.

5. A player controls a pile in the field if their animal is on top (i.e., Milo controls all the piles with a kitten on top, and Otis all the piles with a puppy on top). The first player to control K piles in the field wins, where K=5.
-}

import Data.IORef
import Control.Monad
import Graphics.UI.GLUT

data Params = Params { gPieces :: Int, gThresh :: Int, gHeight :: Int }

standard :: Params
standard = Params { gPieces = 8, gThresh = 5, gHeight = 3 }

data Animal = Chatul | Kelev
  deriving (Eq, Show)
type Pile = [Animal]
type Board = (Int, [Pile], Int)
type FieldPos = Int
data GIndex = Chez Animal | Field FieldPos
  deriving (Show, Eq)
newtype Move = PP(GIndex,FieldPos)
  deriving Eq
data State = State { board :: Board, turn :: Animal, prev :: Maybe Move,
                             highlight :: Maybe GIndex,
                             debug :: String }

neg :: Animal -> Animal
neg Chatul = Kelev
neg Kelev = Chatul

caseAnimal :: Animal -> c -> c -> c
caseAnimal Chatul x y = x
caseAnimal Kelev x y = y

spiles :: State -> [Pile]
spiles s = let (_,piles,_) = board s in piles

owner :: Pile -> Maybe Animal
owner [] = Nothing
owner (a:_) = Just a

assign :: [Pile] -> ([Pile],[Pile]) -> ([Pile],[Pile])
assign [] (cs,ds) = (cs,ds)
assign (p:ps) (cs,ds) = case owner p of
       Nothing -> assign ps (cs,ds)
       Just Chatul -> assign ps (p:cs,ds)
       Just Kelev -> assign ps (cs,p:ds)

pop :: GIndex -> Board -> Maybe (Board,Animal)
pop (Chez Chatul) (cats,field,dogs) =
    if cats > 0 then Just ((cats-1,field,dogs), Chatul) else Nothing
pop (Chez Kelev) (cats,field,dogs) =
    if dogs > 0 then Just ((cats,field,dogs-1), Kelev) else Nothing
pop (Field i) (cats,field,dogs) =
    case splitAt i field of
         (ps, []) -> Nothing
         (ps, []:_) -> Nothing
         (ps, (a:as):ps') -> Just ((cats, ps ++ as:ps', dogs), a)

push :: Int -> (Board,Animal) -> Board 
push i ((cats, field, dogs), a) =
     case splitAt i field of
          (ps, []) -> error "push out of bounds"
          (ps, p:ps') -> (cats, ps ++ (a:p):ps', dogs)

isCool :: State -> FieldPos -> Bool
isCool s j = case prev s of
      Just (PP (_,p)) -> j /= p
      Nothing -> True

legalMoves :: State -> [Move]
legalMoves s =
           let (cats,piles,dogs) = board s in
           let npiles = length piles in
           let player = turn s in
           let myPieces = caseAnimal player cats dogs in
           let entering = [PP (Chez player, i) | myPieces > 0, i <- [0..npiles], length (piles !! i) < gHeight standard] in
           let moving = [PP (Field j,i) | i <- [0..npiles], j <- [0..npiles], i /= j, length (piles !! j) > 0, length (piles !! i) < gHeight standard, isCool s j] in
           entering ++ moving

doMove :: Move -> State -> State
doMove (PP (x, i)) s =
       let Just (board',a) = pop x (board s) in
       let board'' = push i (board',a) in
       s { board = board'', prev = Just (PP (x,i)), turn = neg (turn s), debug = "did move" ++ show x ++ " -> " ++ show i }

checkLegal :: State -> Move -> a -> (String -> a) -> a
checkLegal s (PP (Chez Chatul, i)) ks kf =
        let (cats,piles,dogs) = board s in
        if turn s /= Chatul then kf ("Kelev can't take from Chatul's house") else
        if cats == 0 then kf ("No chatlulim left in house") else
        if length (piles !! i) >= gHeight standard then kf ("pile too high") else
        ks
checkLegal s (PP (Chez Kelev, i)) ks kf =
        let (cats,piles,dogs) = board s in
        if turn s /= Kelev then kf ("Chatul can't take from Kelev's house") else
        if dogs == 0 then kf ("No klalavim left in house") else
        if length (piles !! i) >= gHeight standard then kf ("pile too high") else
        ks
checkLegal s (PP (Field j, i)) ks kf =
        let (cats,piles,dogs) = board s in
        if i == j then kf ("self-move") else
        if length (piles !! j) == 0 then kf ("empty pile") else
        if length (piles !! i) >= gHeight standard then kf ("pile too high") else
        if not (isCool s j) then kf ("hot piece") else
        ks

isLegal :: State -> Move -> Bool
isLegal s m = checkLegal s m True (\_ -> False)

color3f r g b = color $ Color3 r g (b :: GLfloat)
color4f r g b a = color $ Color4 r g (b :: GLfloat) a
vertex3f x y z = vertex $ Vertex3 x y (z :: GLfloat)
vector3 x y z = Vector3 x y (z :: GLfloat)

main :: IO ()
main = do
  (progName,_) <- getArgsAndInitialize
  initialWindowSize $= Size 800 800

  state <- newIORef (State { board = (gPieces standard, take (2 * gThresh standard) (repeat []), gPieces standard),
                                         turn = Chatul,
                                         prev = Nothing, highlight = Nothing,
                                         debug = "init" })
  createWindow progName

  displayCallback $= displayState state
  keyboardMouseCallback $= Just (keyboard state)
  mainLoop

type Coord2d = (GLfloat,GLfloat)

recenter :: Coord2d -> IO ()
recenter (x,y) = do
  loadIdentity
  translate $ vector3 x y 0

scal :: GLfloat -> GLfloat -> GLfloat -> IO ()
scal x y z = scale x y z

sphere r xs ys zs = do
  preservingMatrix $ do
    scal xs ys zs
    createSphere r
 where
  createSphere r = renderObject Solid $ Sphere' r 50 50

renderEyes :: IO ()
renderEyes = preservingMatrix $ do
  translate $ vector3 (-0.03) 0 0
  color3f 1 1 0.9
  sphere 0.02 1 1 1
  color3f 0.1 0.1 0
  sphere 0.005 1 1 1
  translate $ vector3 0.06 0 0
  color3f 1 1 0.9
  sphere 0.02 1 1 1
  color3f 0.1 0.1 0
  sphere 0.005 1 1 1

-- colors

pieceColor = color3f 0.3 0.2 0.2
blackColor = color3f 0 0 0
fieldColor = color3f 0.3 0.8 0.3
boardColor = color3f 0.9 0.7 0.7
arrowColor = color3f 0.2 0.9 0.9

fieldRadius :: GLdouble
fieldRadius = 0.75
fieldRadiusF :: GLfloat
fieldRadiusF = 0.75

renderCat :: IO ()
renderCat  = preservingMatrix $ do
  blackColor
  sphere 0.14 1 1 1
  pieceColor
  sphere 0.13 1 1 1
  scal 0.8 0.8 0.8
  translate $ vector3 0 (-0.01) 0
  color3f 0.8 0.4 0.4
  sphere 0.1 1 1 1
  renderPrimitive Triangles $ do -- draw ears
      vertex3f (-0.07) 0.07 0
      vertex3f (-0.04) 0.15 0
      vertex3f (-0.01) 0.07 0
      vertex3f (0.01) 0.07 0
      vertex3f (0.04) 0.15 0
      vertex3f (0.07) 0.07 0
  translate $ vector3 0 0.04 0
  renderEyes
  translate $ vector3 0 (-0.1) 0
  color3f 0 0 0
  renderPrimitive LineStrip $ do
    vertex3f (-0.05) 0.03 0
    vertex3f 0 0 0
    vertex3f 0.05 0.03 0

renderDog :: IO ()
renderDog = preservingMatrix $ do
  blackColor
  sphere 0.14 1 1 1
  pieceColor
  sphere 0.13 1 1 1
  scal 0.8 0.8 0.8
  translate $ vector3 0 (-0.01) 0
  color3f 0.8 0.8 0.4
  sphere 0.1 0.8 1.2 1
  preservingMatrix $ do
    translate $ vector3 (-0.1) 0.03 0
    rotate 60 $ vector3 0 0 1
    sphere 0.05 1.2 0.4 1
  preservingMatrix $ do
    translate $ vector3 0.1 0.03 0
    rotate (-60) $ vector3 0 0 1
    sphere 0.05 1.2 0.4 1
  translate $ vector3 0 0.04 0
  renderEyes
  translate $ vector3 0 (-0.1) 0
  color3f 0 0 0
  renderPrimitive LineStrip $ do
    vertex3f (-0.05) 0.03 0
    vertex3f 0 0 0
    vertex3f 0.05 0.03 0

renderAnimal :: Animal -> IO ()
renderAnimal Chatul = renderCat
renderAnimal Kelev = renderDog

tau = 2 * pi

renderPile :: Pile -> Coord2d -> IO ()
renderPile pile (x,y) = preservingMatrix $ do
    recenter (x,y)
    forM_ (reverse pile) (\a -> do
               renderAnimal a
               translate $ vector3 0 0.02 0)

pileCoord :: Int -> GIndex -> Coord2d
pileCoord tot (Field i) =
          let angle = fromIntegral i * tau / fromIntegral tot in
          (0.75 * fieldRadiusF * cos angle, 0.75 * fieldRadiusF * sin angle)
pileCoord tot (Chez Chatul) = (-0.8,-0.7)
pileCoord tot (Chez Kelev) = (0.8,-0.7)

coordPile :: Int -> Coord2d -> Maybe FieldPos
coordPile tot (x,y) | x^2 + y^2 < fieldRadiusF^2 =
          let theta = atan(y/x) in
          let theta' = case (x,y) of
                 (x,y) | x>=0 && y>=0 -> theta
                 (x,y) | x<0 && y>=0 -> theta + tau/2
                 (x,y) | x<0 && y<0 -> theta + tau/2
                 (x,y) -> theta + tau
          in
          Just (round (fromIntegral tot * theta' / tau) `mod` tot)
coordPile _ _ = Nothing

renderPiles :: [Pile] -> IO ()
renderPiles piles = preservingMatrix $ do
    translate $ vector3 (-0.3) 0 0
    forM_ (zip piles [0..length piles-1]) (\(p,i) -> renderPile p (pileCoord (length piles) (Field i)))

renderLArrow :: IO ()
renderLArrow = do
  renderPrimitive Quads $
                  do
                        vertex3f (-0.3) 0.05 0
                        vertex3f 0.3 0.05 0
                        vertex3f 0.3 (-0.05) 0
                        vertex3f (-0.3) (-0.05) 0
  renderPrimitive Polygon $
                  do
                        vertex3f (-0.3) 0.05 0
                        vertex3f (-0.3) 0.15 0
                        vertex3f (-0.5) 0 0
                        vertex3f (-0.3) (-0.15) 0
                        vertex3f (-0.3) (-0.05) 0

renderRArrow :: IO ()
renderRArrow = do
  rotate 180 $ vector3 0 0 1
  renderLArrow

renderState :: State -> IO ()
renderState state = do
  let (cats,piles,dogs) = board state
  let npiles = length piles
  loadIdentity
  boardColor
  renderPrimitive Quads $
               do
                  vertex3f (-1) 1 0
                  vertex3f 1 1 0
                  vertex3f 1 (-1) 0
                  vertex3f (-1) (-1) 0

  recenter (0,0)
  fieldColor
  sphere fieldRadius 1 1 1
  renderPiles piles

  renderPile (take cats (repeat Chatul)) (pileCoord npiles (Chez Chatul))
  renderPile (take dogs (repeat Kelev)) (pileCoord npiles (Chez Kelev))

  -- show a highlighting mark
  blend $= Enabled
  blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
  (case highlight state of
       Just x -> do
            recenter (pileCoord npiles x)
            color4f 1 1 0 0.1
            sphere 0.1 1 1 1
       Nothing -> return ())
  blend $= Disabled

  -- indicate the number of piles controlled by each player
  let (cs,ds) = assign piles ([],[])
  recenter (-0.9,0.8)
  color3f 1 1 0
  scal 0.001 0.001 0.001
  renderString Roman (show (length cs))
  recenter (0.9,0.8)
  color3f 1 1 0
  scal 0.001 0.001 0.001
  renderString Roman (show (length ds))

  -- render an arrow indicating whose turn it is
  recenter (0,0.9)
  scal 0.6 0.6 0.6
  arrowColor
  case turn state of 
       Chatul -> renderLArrow
       Kelev -> renderRArrow

displayState :: IORef State -> DisplayCallback
displayState state = do
  s <- get state

  clearColor $= Color4 0 0 0 1
  clear [ColorBuffer]

  renderState s

  recenter (-0.9,0.7)
  color3f 0 0 0
  scal 0.0002 0.0002 0.0002
  renderString Roman ("debug: " ++ debug s) 

  -- reset button
  recenter (0,-0.85)
  color3f 0.6 0.4 0.8
  renderPrimitive Quads $ do
    vertex3f (-0.2) (-0.1) 0
    vertex3f 0.2 (-0.1) 0
    vertex3f 0.2 0.07 0
    vertex3f (-0.2) 0.07 0
  recenter (-0.15,-0.9)
  color3f 1 1 0.8
  scal 0.001 0.001 0.001
  renderString Roman ("Reset") 

  flush

pixelRelative :: (GLint, GLint) -> IO Coord2d
pixelRelative (x,y) = do
  Size w h <- get windowSize 
  return (2*fromIntegral x/fromIntegral w-1,
             1-2 *fromIntegral y/fromIntegral h)

coordGIndex :: Int -> Coord2d -> Maybe GIndex
coordGIndex _ (x,y) | x < -0.65 && y < -0.4 = Just (Chez Chatul)
coordGIndex _ (x,y) | x > 0.65 && y < -0.4 = Just (Chez Kelev)
coordGIndex tot (x,y) = coordPile tot (x,y) >>= return . Field
-- coordGIndex (x,y) = Nothing

keyboard :: IORef State -> KeyboardMouseCallback
keyboard state (MouseButton LeftButton) Down _ (Position x y) =  do
  s <- get state
  let (_,piles,_) = board s
  (x',y') <- pixelRelative (x,y)
  -- check reset button
  if x' > -0.2 && x' < 0.2 && y' > -0.95 && y' < -0.77 then do
     state $= State { board = (gPieces standard, take (2 * gThresh standard) (repeat []), gPieces standard), turn = Chatul, prev = Nothing, highlight = Nothing, debug = "reset" }
     postRedisplay Nothing
  else do
  let p = coordGIndex (length piles) (x',y')
  let locstring = "loc = " ++ show p -- ++  "abs = " ++ show (x,y) ++ " rel = " ++ show (x',y')
  case (highlight s,p) of
    (Nothing,p) -> state $= s { highlight = p, debug = locstring }
    (Just h,Just (Field i)) ->
          checkLegal s (PP (h,i))
                     (state $= (doMove (PP (h,i)) s) { highlight = Nothing })
                     (\err -> state $= s { highlight = Nothing, debug = "illegal move!: " ++ show h ++ " -> " ++ show p ++ " at " ++ locstring ++ "(" ++ err ++ ")" })
    (Just h,_) -> state $= s { highlight = Nothing, debug = "non pile, " ++ locstring }
  postRedisplay Nothing
keyboard _ _ _ _ _ = return ()
