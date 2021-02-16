import System.IO
import System.Exit (exitSuccess)

type Screen = String            -- 23 lines, 79 chars each (79 + '\n' = 80)
data Event = KeyPress String

type DrawFun = Integer -> Integer -> Char
type Picture = DrawFun -> DrawFun

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
        handle' (KeyPress "r") _ = initialState
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
        handle' (KeyPress "n") (VictoryScreen state) = Running $ nextLevel state
        handle' _ (VictoryScreen state) = VictoryScreen state
        draw' StartScreen = startScreen
        draw' (Running state) = draw state
        draw' (VictoryScreen state) = victoryScreen $ getScore state

withUndo :: Eq s => Activity s -> Activity (WithUndo s)
withUndo (Activity initialState handle draw) =
    Activity initialState' handle' draw'
    where
        initialState' = WithUndo initialState []
        handle' (KeyPress "u") (WithUndo state stack) =
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
        info = lettering $ "level " ++ show (level state + 1) ++ replicate 10 ' ' ++ show (numberOfMoves state) ++ " moves"
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


main :: IO ()
main = etap5



-------------------
-- CodeWorld API
-------------------

blank :: Picture
blank = id

infixr 0 &
(&) :: Picture -> Picture -> Picture
(&) = (.)

pictures :: [Picture] -> Picture
pictures pics = foldr (&) blank pics

translated :: Integer -> Integer -> Picture -> Picture
translated dx dy picture =
    shift (-) . picture . shift (+)
    where
        shift :: (Integer -> Integer -> Integer) -> Picture
        shift op drawFun x y = drawFun (op x dx) (op y dy)

lettering :: String -> Picture
lettering str drawFun x 0
    | x < before - 39 = drawFun x 0
    | x > 39 - after  = drawFun x 0
    | otherwise       = nth str (x + 39 - before)
    where
        strLen = listLength str
        before = (79 - strLen) `div` 2
        after = 79 - strLen - before
lettering str drawFun x y = drawFun x y

activityOf :: world -> (Event -> world -> world) -> (world -> Picture) -> IO ()
activityOf initialState handle draw = do
    hSetBuffering stdin NoBuffering
    input <- getContents
    clearScreen
    putPicture $ draw initialState
    go initialState input
    where
        go _ ('q':_) = quit
        go state ('\ESC':'[':'A':t) = go' state "Up" t
        go state ('\ESC':'[':'B':t) = go' state "Down" t
        go state ('\ESC':'[':'D':t) = go' state "Left" t
        go state ('\ESC':'[':'C':t) = go' state "Right" t
        go state (h:t) = go' state [h] t
        go' state key t = do
            let newState = handle (KeyPress key) state
            clearScreen
            putPicture $ draw newState
            go newState t
        putPicture :: Picture -> IO ()
        putPicture = putScreen . showPicture

quit :: IO ()
quit = clearScreen >> putStrLn "bye!" >> exitSuccess

clearScreen :: IO ()
clearScreen = putStr "\ESCc"

showPicture :: Picture -> Screen
showPicture picture =
    [if x == 40 then '\n' else p x y | y <- [11,10..(-11)], x <- [-39..40]]
    where
        p x y = picture (\_ _ -> ' ') x y

putScreen :: Screen -> IO ()
putScreen screen = putStr "\ESC[?25l" >> putStr screen >> putStr "\ESC[?25h"



-------------
-- screens
-------------

startScreen :: Picture
startScreen = lettering "Sokoban!" & translated 0 (-2) (lettering "press space to start the game")

victoryScreen :: Integer -> Picture
victoryScreen numberOfMoves = lettering "🎉 Victory! 🎊" & translated 0 (-2) (lettering $ "level completed in " ++ show numberOfMoves ++ " moves") & translated 0 (-4) (lettering "press n to play next level")



--------------
-- pictures
--------------

player :: Direction -> Picture
player _ _ 0 0 = '@'
player _ drawFun x y = drawFun x y

wall, ground, storage, box :: Picture

wall _ 0 0 = '#'
wall drawFun x y = drawFun x y

ground _ 0 0 = ' '
ground drawFun x y = drawFun x y

storage _ 0 0 = '.'
storage drawFun x y = drawFun x y

box _ 0 0 = '$'
box drawFun x y = drawFun x y


atCoords :: Coords -> Picture -> Picture
atCoords (C x y) picture = translated x y picture

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
            | x == 1 && y == -2        = Ground
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
            | x == 1 && y == 2         = Box
            | x == 1 && y == 4         = Storage
            | x == 0                   = Wall
            | otherwise                = Ground
    in fun)]

allMazes :: [Maze]
allMazes = appendList mazes badMazes
