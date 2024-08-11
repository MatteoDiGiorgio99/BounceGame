import System.Random
import Control.Monad

class (Show a) => Actor a where
    move :: String -> [a] -> a -> [a]
    rect :: a -> (Int, Int, Int, Int)  -- (x, y, w, h)
    collide :: a -> a -> a

data Arena a = Arena { actors :: [a]
                     } deriving (Show)

tick :: (Actor a) => Arena a -> String -> Arena a
tick (Arena actors) keys = Arena (listAfterConflicts (foldl (move keys) actors actors))

operateArena :: (Actor a) => Arena a -> IO ()
operateArena arena = do
    print arena
    line <- getLine
    when (line /= "q") $ operateArena (tick arena line)

checkCollision :: (Actor a) => a -> a -> Bool
checkCollision a1 a2 = (rect a1) /= (rect a2) && y2 < y1+h1 && y1 < y2+h2 && x2 < x1+w1 && x1 < x2+w2
    where
        (x1, y1, w1, h1) = rect a1
        (x2, y2, w2, h2) = rect a2

seeksConflicts :: (Actor a) => [a] -> [a] -> [a]
seeksConflicts [] _ = []
seeksConflicts [a] acc = a:acc
seeksConflicts (x:xs) acc =
    let clash = filter (checkCollision x) xs
        refresh = foldl collide x clash
    in seeksConflicts xs (refresh:acc)

{-
isolate' :: [a] -> [a] -> [([a], a)]
isolate' left [] = []
isolate' left (v:right) = (left++right, v) : isolate' (v:left) right
isolate = isolate' []
-}

listAfterConflicts :: (Actor a) => [a] -> [a]
listAfterConflicts actors = seeksConflicts (seeksConflicts actors []) []

--Metodo per non far uscire dal campo da gioco gli attori
clamp :: Int -> Int -> Int
clamp value limit = max 0 (min value limit)

newActors :: BasicActor -> BasicActor -> [BasicActor] -> [BasicActor]
newActors ancientActor insertActor = map update
 where update actor
        | actor == ancientActor = insertActor
        | otherwise = actor

maxX = 320
maxY = 240
actorW = 20
actorH = 20

data BasicActor = Ball { x :: Int, y :: Int, dx :: Int, dy :: Int }
                | Ghost { x :: Int, y :: Int, rnd :: StdGen, counter :: Int }  -- aggiunto contatore mosse per generare nuove palline
                | Turtle { x :: Int, y :: Int, dead :: Bool} deriving (Show)



--Classe Eq definisce uguaglianze e disuguaglianze
instance Eq BasicActor where
    (Ball x y _ _) == (Ball x' y' _ _) = x == x' && y == y'
    (Ghost x y _ _) == (Ghost x' y' _ _) = x == x' && y == y'
    (Turtle x y _) == (Turtle x' y' _) = x == x' && y == y'
    _ == _ = False

instance Actor BasicActor where
    
    collide :: BasicActor -> BasicActor -> BasicActor
    collide (Ball x y dx dy) (Ball x' y' _ _) = Ball x y (-dx) (-dy)    -- Le palline rimbalzano
    collide (Ball x y dx dy) (Turtle _ _ _) = Ball x y (-dx) (-dy)      -- La pallina rimbalza e la tartaruga muore
    collide (Turtle x y _) (Ball _ _ _ _) = Turtle x y True             -- La tartaruga muore
    collide a _ = a 
    
    rect (Ball x y _ _) = (x, y, actorW, actorH)
    rect (Ghost x y _ _) = (x, y, actorW, actorH)
    rect (Turtle x y _) = (x, y, actorW, actorH)

    move keys actors (Ball x y dx dy) =
        let dx' = if x + dx < actorW || x + dx > maxX - actorW   --rimbalzo sui bordi
                then -dx
                else dx
            dy' = if y + dy < actorH || y + dy > maxY - actorH
                then -dy
                else dy

            newX = clamp (x + dx') maxX -- coordinate interne all'arena
            newY = clamp (y + dy') maxY

            oldBall = Ball x y dx dy
            updatedBall = Ball newX newY dx' dy'
            --collidedActors = filter (checkCollision updatedBall) actors
            --newActors = foldl collide updatedBall collidedActors
        in newActors oldBall updatedBall actors

    move keys actors (Ghost x y rnd counter)
        | counter >= 7 = let (var, newRnd) = randomR (1,-1) rnd -- Ogni 7 movimenti della tartaruga crea una nuova pallina
                             x' = (x + 5 * var) `mod` maxX      -- Spawn/movimento Fantasma  "Effetto Pac-Man"
                             y' = (y + 5 * var) `mod` maxY

                             oldGhost = Ghost x y rnd counter
                             newGhost = Ghost x' y' newRnd 0
                             newBall = Ball x' y' 5 5           -- Genera una nuova pallina
                             updateActors = newBall : actors    -- Lista con nuova pallina
                         in newActors oldGhost newGhost updateActors

        | otherwise = let (var, newRnd) = randomR (1,-1) rnd
                          x' = (x + 5 * var) `mod` maxX         -- Spawn/movimento Fantasma  "Effetto Pac-Man"
                          y' = (y + 5 * var) `mod` maxY

                          oldGhost = Ghost x y rnd counter
                          newGhost = Ghost x' y' newRnd (counter + 1)
                      in newActors oldGhost newGhost actors

    move keys actors (Turtle x y True) = filter (not . isTurtle) actors       -- Tartaruga morta, restituisce lista senza tartaruga   
         where isTurtle (Turtle {}) = True
               isTurtle _ = False                       
    move keys actors (Turtle x y False) =
        let dx'
             | 'd' `elem` keys = 5         -- Movimenti tartaruga
             | 'a' `elem` keys = -5
             | otherwise = 0
            dy'
             | 's' `elem` keys = 5
             | 'w' `elem` keys = -5
             | otherwise = 0

            newX = clamp (x + dx') maxX
            newY = clamp (y + dy') maxY

            oldTurtle = Turtle x y False
            updatedTurtle = Turtle newX newY False
            --collideWith = filter (checkCollision updatedTurtle) actors
            --newActors = foldl collide updatedTurtle collideWith
         in newActors oldTurtle updatedTurtle actors

main :: IO ()
main = do
    rnd <- newStdGen
    operateArena (Arena [Ball 100 90 5 5, Ball 130 120 (-5) (-5), Ghost 100 100 rnd 0, Turtle 200 110 False])
