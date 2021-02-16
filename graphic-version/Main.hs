{-# LANGUAGE OverloadedStrings #-}

import CodeWorld
import Data.Text (pack)

data Tile = Wall | Ground | Storage | Box | Blank deriving Eq
data Direction = U | D | L | R deriving Eq
data Coords = C Integer Integer deriving Eq
data Maze = Maze Coords (Coords -> Tile)

data State = State
    { playerPosition  :: Coords
    , playerDirection :: Direction
    , boxes           :: [Coords]
    , level           :: Integer
    , numberOfMoves   :: Integer
    } deriving Eq

data Activity world = Activity
    world                       -- initial state
    (Event -> world -> world)   -- event handler
    (world -> Picture)          -- visualizer

data WithStartAndVictoryScreens world = StartScreen | Running world | VictoryScreen world
data WithUndo a = WithUndo a [a]


runActivity :: Activity s -> IO ()
runActivity (Activity initialState handle draw) = activityOf initialState handle draw

resettable :: Activity s -> Activity s
resettable (Activity initialState handle draw) =
    Activity initialState handle' draw
    where
        handle' (KeyPress "Esc") state = state
        handle' (KeyRelease "Esc") _ = initialState
        handle' event state = handle event state

withStartAndVictoryScreens :: (s -> Bool) -> (s -> Integer) -> (s -> s) -> Activity s -> Activity (WithStartAndVictoryScreens s)
withStartAndVictoryScreens victoryVerifier getScore nextLevel (Activity initialState handle draw) =
    Activity initialState' handle' draw'
    where
        initialState' = StartScreen
        handle' (KeyPress " ") StartScreen = Running initialState
        handle' _ StartScreen = StartScreen
        handle' event (Running state) =
            let
                newState = handle event state
                victory = victoryVerifier newState
            in
            if victory then VictoryScreen newState else Running newState
        handle' (KeyPress "N") (VictoryScreen state) = Running $ nextLevel state
        handle' _ (VictoryScreen state) = VictoryScreen state
        draw' StartScreen = startScreen
        draw' (Running state) = draw state
        draw' (VictoryScreen state) = victoryScreen $ getScore state

withUndo :: Eq s => Activity s -> Activity (WithUndo s)
withUndo (Activity initialState handle draw) =
    Activity initialState' handle' draw'
    where
        initialState' = WithUndo initialState []
        handle' (KeyPress "U") (WithUndo state stack) =
            case stack of
                state':stack' -> WithUndo state' stack'
                [] -> WithUndo state []
        handle' event (WithUndo state stack)
            | newState == state = WithUndo state stack
            | otherwise = WithUndo newState (state:stack)
            where
                newState = handle event state
        draw' (WithUndo state _) = draw state


maze :: State -> Maze
maze state = if level state < listLength mazes then nth mazes $ level state else nth badMazes 2

adjacentCoords :: Direction -> Coords -> Coords
adjacentCoords U (C x y) = C x (y + 1)
adjacentCoords D (C x y) = C x (y - 1)
adjacentCoords L (C x y) = C (x - 1) y
adjacentCoords R (C x y) = C (x + 1) y

removeBoxes :: Maze -> Maze
removeBoxes (Maze playerPosition tileFun) =
    Maze playerPosition (boxToGround . tileFun)
    where
        boxToGround :: Tile -> Tile
        boxToGround Box = Ground
        boxToGround tile = tile

addBoxes :: [Coords] -> Maze -> Maze
addBoxes coordsList (Maze playerPosition tileFun) =
    Maze playerPosition tileFun'
    where
        tileFun' c = if elem c coordsList then Box else tileFun c

boxesCoords :: Maze -> [Coords]
boxesCoords (Maze playerPosition tileFun) = filterList ((== Box) . tileFun) $ reachableVertices playerPosition (neighboursCoords tileFun)

handleEvent :: Event -> State -> State
handleEvent event state =
    case event of
        KeyPress "Up" -> tryToMove U
        KeyPress "Down" -> tryToMove D
        KeyPress "Left" -> tryToMove L
        KeyPress "Right" -> tryToMove R
        _ -> state
    where
        tryToMove :: Direction -> State
        tryToMove direction =
            let
                currentPosition = playerPosition state
                targetPosition = adjacentCoords direction currentPosition
                Maze _ tileFunWithBoxes = addBoxes (boxes state) (removeBoxes $ maze state)
            in
            case tileFunWithBoxes targetPosition of
                Ground -> state { playerPosition = targetPosition, playerDirection = direction, numberOfMoves = numberOfMoves state + 1 }
                Storage -> state { playerPosition = targetPosition, playerDirection = direction, numberOfMoves = numberOfMoves state + 1 }
                Box ->
                    let
                        targetBoxPosition = adjacentCoords direction targetPosition
                        legalMove = (\tile -> tile == Ground || tile == Storage) $ tileFunWithBoxes targetBoxPosition
                        moveBox :: Coords -> Coords
                        moveBox c = if c == targetPosition then targetBoxPosition else c
                    in
                    if legalMove
                    then
                        state { playerPosition = targetPosition, playerDirection = direction, boxes = map moveBox $ boxes state, numberOfMoves = numberOfMoves state + 1 }
                    else
                        state { playerDirection = direction }
                _ -> state { playerDirection = direction }

draw :: State -> Picture
draw state =
    let
        playerPosition' :: Coords
        playerPosition' = playerPosition state
        player' :: Picture
        player' = player $ playerDirection state
        info :: Picture
        info = lettering . pack $ "level " ++ show (level state + 1) ++ replicate 10 ' ' ++ show (numberOfMoves state) ++ " moves"
    in
    atCoords playerPosition' player' & pictureOfBoxes (boxes state) & pictureOfMaze (removeBoxes $ maze state) & translated 0 (-8) info

isWinning :: State -> Bool
isWinning state =
    all ((== Storage) . tileFun) $ boxes state
    where
        Maze _ tileFun = maze state

nextLevel :: State -> State
nextLevel state =
    State playerPosition R (boxesCoords maze) newLevel 0
    where
        newLevel = level state + 1
        maze@(Maze playerPosition _) = if newLevel < listLength mazes then nth mazes newLevel else nth badMazes 2

unpackWithUndo :: WithUndo State -> State
unpackWithUndo (WithUndo state _) = state

etap5 :: IO ()
etap5 =
    runActivity $ resettable . withStartAndVictoryScreens isWinning' numberOfMoves' nextLevel' . withUndo $ activity
    where
        activity :: Activity State
        activity = Activity initialState handleEvent draw
        maze@(Maze playerPosition _) = head mazes
        initialState :: State
        initialState = State playerPosition R (boxesCoords maze) 0 0
        isWinning' = isWinning . unpackWithUndo
        numberOfMoves' = numberOfMoves . unpackWithUndo
        nextLevel' (WithUndo state _) = WithUndo (nextLevel state) []

neighboursCoords :: (Coords -> Tile) -> Coords -> [Coords]
neighboursCoords tileFun c =
    filterList ((/= Wall) . tileFun) $ mapList (flip adjacentCoords c) [U, D, L, R]

isClosed :: Maze -> Bool
isClosed (Maze playerPosition tileFun) =
    (tileFun playerPosition == Ground || tileFun playerPosition == Storage) &&
        isGraphClosed playerPosition (neighboursCoords tileFun) ((/= Blank) . tileFun)

isSane :: Maze -> Bool
isSane (Maze playerPosition tileFun) =
    listLength reachableStorages >= listLength reachableBoxes
    where
        reachableTiles = mapList tileFun $ reachableVertices playerPosition (neighboursCoords tileFun)
        reachableStorages = filterList (== Storage) reachableTiles
        reachableBoxes = filterList (== Box) reachableTiles

etap4 :: Picture
etap4 = translated (-5) 0 (lettering "closed:") & translated (-2) 0 (pictureOfBools $ map isClosed allMazes) &
        translated 2 0 (lettering "sane:") & translated 5 0 (pictureOfBools $ map isSane allMazes)


main :: IO ()
main = etap5
-- main = drawingOf etap4
-- main = drawingOf . pictureOfReachableCoords $ nth badMazes 3



-------------
-- screens
-------------

startScreen :: Picture
startScreen = scaled 3 3 (styledLettering Italic Handwriting "Sokoban!") & translated 0 (-2) (styledLettering Plain Monospace "press space to start the game") & translated 0 (-4) etap4

victoryScreen :: Integer -> Picture
victoryScreen numberOfMoves = scaled 3 3 (lettering "🎉 Victory! 🎊") & translated 0 (-2) (lettering . pack $ "level completed in " ++ show numberOfMoves ++ " moves") & translated 0 (-4) (styledLettering Plain Monospace "press N to play next level")



--------------
-- pictures
--------------

player :: Direction -> Picture
player direction =
    case direction of
        U -> rotated (pi / 2) playerR
        D -> reflected 0 . rotated (pi / 2) $ playerR
        L -> reflected (pi / 2) playerR
        R -> playerR
    where
        playerR, head, torso, hands, legs, arrow :: Picture
        playerR = head & torso & hands & legs & arrow
        head = translated 0 0.3 $ solidCircle 0.15
        torso = thickPolyline 0.05 [(0, 0.3), (0, -0.22)]
        hands = thickPolyline 0.05 [(-0.05, 0.09), (0.2, -0.08)] & thickPolyline 0.05 [(0, 0.09), (0.22, 0.1)]
        legs = thickPolyline 0.05 [(-0.15, -0.45), (0, -0.22), (0.15, -0.45)]
        arrow = colored (light blue) $ solidPolygon [(0.3, -0.3), (0.3, 0.3), (0.45, 0)]

wall, ground, storage, box :: Picture
wall =
    bricks & background (dark gray)
    where
        bricks, brick :: Picture
        bricks = clipped 1 1 . pictures $ map (\(x, y) -> translated x y brick) directions
        brick = translated 0.25 0.125 $ rectangle 0.5 0.25
        directions :: [(Double, Double)]
        directions = [(-0.5, 0.25), (0, 0.25), (-0.75, 0), (-0.25, 0), (0.25, 0), (-0.5, -0.25), (0, -0.25), (-0.75, -0.5), (-0.25, -0.5), (0.25, -0.5)]
ground = dots (lighter 0.3 brown) & background (lighter 0.4 yellow)
storage =
    goal & dots (lighter 0.3 green) & background (lighter 0.4 green)
    where
        goal :: Picture
        goal = colored red $ pictures [thickCircle 0.08 r | r <- [0.02, 0.13, 0.25]]
box =
    border black & wood & background orange
    where
        wood, board :: Picture
        wood = colored (dark brown) $ (thickRectangle 0.03 0.7 0.7) & clipped 0.7 0.7 $ (rotated (-pi / 4) board) & (rotated (pi / 4) board)
        board = thickRectangle 0.03 0.15 1.5

background :: Color -> Picture
background color = colored color $ solidRectangle 1 1

border :: Color -> Picture
border color = colored color $ thickRectangle 0.03 1 1

dots :: Color -> Picture
dots color =
    colored color . clipped 1 1 $ pictures [translated (fromIntegral x / 8) (fromIntegral y / 8) dot | x <- [-4..4], y <- [-4..4]]
    where
        dot :: Picture
        dot = solidCircle 0.02

atCoords :: Coords -> Picture -> Picture
atCoords (C x y) picture = translated (fromIntegral x) (fromIntegral y) picture

drawTile :: Tile -> Picture
drawTile Wall    = wall
drawTile Ground  = ground
drawTile Storage = storage
drawTile Box     = box
drawTile Blank   = blank

pictureOfMaze :: Maze -> Picture
pictureOfMaze (Maze playerPosition tileFun) = pictures $ map (\c -> atCoords c . drawTile . tileFun $ c) [C x y | x <- [-10..10], y <- [-10..10]]

pictureOfBoxes :: [Coords] -> Picture
pictureOfBoxes coordsList = pictures $ map (\c -> atCoords c $ drawTile Box) coordsList

pictureOfBools :: [Bool] -> Picture
pictureOfBools xs = translated (-fromIntegral (k - 1) / 2) (fromIntegral (k - 1) / 2) (go 0 xs)
    where
        n = length xs
        k = findK 0 -- k is the integer square of n
        findK i | i * i >= n = i
                | otherwise  = findK (i+1)
        go _ [] = blank
        go i (b:bs) =
            translated (fromIntegral (i `mod` k))
                       (-fromIntegral (i `div` k))
                       (pictureOfBool b)
            & go (i + 1) bs
        pictureOfBool True =  colored green (solidCircle 0.3)
        pictureOfBool False = colored red   (solidCircle 0.3)

pictureOfReachableCoords :: Maze -> Picture
pictureOfReachableCoords maze@(Maze playerPosition tileFun) =
    pictureOfBools bools & atCoords playerPosition (player R) & pictureOfMaze maze
    where
        bools = map (\c -> reachable c playerPosition $ neighboursCoords tileFun) [C x y | y <- [10,9..(-10)], x <- [-10..10]]


--------------------
-- list functions
--------------------

elemList :: Eq a => a -> [a] -> Bool
elemList x xs = foldList ((||) . (== x)) False xs

appendList :: [a] -> [a] -> [a]
appendList xs ys = foldList (:) ys xs

listLength :: [a] -> Integer
listLength xs = foldList (\x acc -> acc + 1) 0 xs

filterList :: (a -> Bool) -> [a] -> [a]
filterList f xs = foldList (\x acc -> if f x then x : acc else acc) [] xs

nth :: [a] -> Integer -> a
nth xs n
    | n < 0     = error "nth: negative index"
    | len <= n  = error "nth: too large index"
    | otherwise = snd $ foldList (\x (i, ret) -> (i - 1, if i == n then x else ret)) (len - 1, error "nth: empty list") xs
    where
        len = listLength xs

mapList :: (a -> b) -> [a] -> [b]
mapList f xs = foldList ((:) . f) [] xs

andList :: [Bool] -> Bool
andList xs = foldList (&&) True xs

allList :: (a -> Bool) -> [a] -> Bool
allList f xs = foldList ((&&) . f) True xs

foldList :: (a -> b -> b) -> b -> [a] -> b
foldList f z [] = z
foldList f z (x:xs) = f x (foldList f z xs)



---------------------
-- graph functions
---------------------

reachableVertices :: Eq a => a -> (a -> [a]) -> [a]
reachableVertices initial neighbours =
    visit initial []
    where
        visit v visited =
            foldList (\u acc -> if elemList u acc then acc else visit u acc) (v:visited) (neighbours v)

isGraphClosed :: Eq a => a -> (a -> [a]) -> (a -> Bool) -> Bool
isGraphClosed initial neighbours isOk = allList isOk $ reachableVertices initial neighbours

reachable :: Eq a => a -> a -> (a -> [a]) -> Bool
reachable v initial neighbours = elemList v $ reachableVertices initial neighbours

allReachable :: Eq a => [a] -> a -> (a -> [a]) -> Bool
allReachable vs initial neighbours = allList (\v -> reachable v initial neighbours) vs



------------
-- levels
------------

mazes :: [Maze]
mazes = map (uncurry Maze) [
    (C (-3) 0,
    let fun (C x y)
            | abs x > 4  || abs y > 4  = Blank
            | abs x == 4 || abs y == 4 = Wall
            | x ==  2 && y <= 0        = Wall
            | x ==  3 && y <= 0        = Storage
            | x >= -2 && y == 0        = Box
            | otherwise                = Ground
    in fun),
    (C (-1) 0,
    let fun (C x y)
            | abs x > 2  || abs y > 1  = Blank
            | abs x == 2 || abs y == 1 = Wall
            | x == 0 && y == 0         = Box
            | x == 1 && y == 0         = Storage
            | otherwise                = Ground
    in fun),
    (C (-2) 0,
    let fun (C x y)
            | abs x > 3  || abs y > 5  = Blank
            | abs x == 3 || abs y == 5 = Wall
            | x == -1 && y == 0        = Box
            | x == 2 && y == 0         = Storage
            | x == 1 && y == -2    = Ground
            | x == 1 && abs y < 4      = Wall
            | otherwise                = Ground
    in fun),
    (C 1 3,
    let fun (C x y)
            | abs x > 3  || abs y > 5  = Blank
            | abs x == 3 || abs y == 5 = Wall
            | x == 0 && y == -3        = Box
            | x == 1 && y == 4         = Storage
            | x == 0 && y > -3         = Wall
            | otherwise                = Ground
    in fun)]

badMazes :: [Maze]
badMazes = map (uncurry Maze) [
    (C (-3) 0,
    let fun (C x y)
            | abs x > 4  || abs y > 4  = Blank
            | abs x == 4 || abs y == 4 = Wall
            | x ==  2 && y <= 0        = Wall
            | x ==  3 && y < 0         = Storage
            | abs x <= 2 && y == 0     = Box
            | otherwise                = Ground
    in fun),
    (C (-2) 0,
    let fun (C x y)
            | abs x > 3  || abs y > 1  = Blank
            | abs x == 3 || abs y == 1 = Wall
            | x == -1 && y == 0        = Box
            | x == 2 && y == 0         = Storage
            | x == 1 && y == 0         = Wall
            | otherwise                = Ground
    in fun),
    (C (-2) 0,
    let fun (C x y)
            | abs x > 3  || abs y > 5  = Blank
            | abs x == 3 || abs y == 5 = Wall
            | x == -1 && y == 0        = Box
            | x == 2 && y == 0         = Storage
            | x == 1 && abs y < 4      = Wall
            | otherwise                = Ground
    in fun),
    (C (-1) 4,
    let fun (C x y)
            | abs x > 2  || abs y > 5  = Blank
            | abs x == 2 || abs y == 5 = Wall
            | x == -1 && y == 2        = Box
            | x == 1 && y == 4         = Storage
            | x == 0                   = Wall
            | otherwise                = Ground
    in fun),
    (C (-1) 4,
    let fun (C x y)
            | abs x > 2  || abs y > 5  = Blank
            | abs x == 2 || abs y == 5 = Wall
            | x == 1 && y == 2        = Box
            | x == 1 && y == 4         = Storage
            | x == 0                   = Wall
            | otherwise                = Ground
    in fun)]

allMazes :: [Maze]
allMazes = appendList mazes badMazes
