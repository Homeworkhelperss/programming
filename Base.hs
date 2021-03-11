{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE NamedFieldPuns        #-}

module Base where

import           Data.List
import qualified System.Console.Haskeline      as SCH
import           Data.Maybe
import           Control.Monad.IO.Class         ( MonadIO(liftIO) )

-- # Core types

-- ## Game State
-- The game state has a player and a room (the current room)
data GameState = GS { player :: Player, room :: Room }

-- The `Next` type is used to represent what happens next in a game
-- either things stay the `Same` or `Progress` is made to a new game state
data Next a =
      Same String       -- Message about why things are staying the same
    | Progress String a -- Success message and next thing `a`

-- ## Player
-- Has a name a list of items
data Player =
  Player
    {   playerName :: String
      , inventory  :: [Item]
    }

-- ## Room (where all the action happens)

data Room =
  Room
    {  name        :: String
     , description :: String
     , isWinRoom   :: Bool
     , requires    :: Maybe Item
       -- ^ whether an item is required to enter this room or not
     , items       :: [(Item, String)]
       -- ^ Association list of items and some description about where they are
     , monsters    :: [Monster]
       -- ^ Some monsters in this room
     , doors       :: [(Direction, Room)]
       -- ^ Association list between directions and a room
     , actions     :: Item -> GameState -> Next GameState
       -- ^ Function taking an item and a game state and returning what to do next
    }

data Direction = North | South | East | West
  deriving Eq

instance Show Direction where
  show North = "north"
  show South = "south"
  show East  = "east"
  show West  = "west"

-- ## Items and monsters
data Item = Key | Spoon
  deriving Eq

instance Show Item where
  show Key   = "key"
  show Spoon = "spoon"

data Monster = WoodTroll { health :: Int, holding :: Item }

instance Show Monster where
  show (WoodTroll health item) = "wood troll holding a " ++ show item

-- ## Command interface

data Command =
  Move Direction | Use Item | PickUp Item | End deriving Eq

-- ## Key type class

class Parsable t where
  parse :: String -> Maybe t

-- ## Helpers for outputing to the user

tellContextLine :: String -> IO ()
tellContextLine s = putStrLn $ "   " ++ s ++ "."

tellDoors :: [(Direction, Room)] -> IO ()
tellDoors []         = tellContextLine $ "There are no doors."
tellDoors [(dir, _)] = tellContextLine $ "There is a door to the " ++ show dir
tellDoors doors =
  tellContextLine
    $  "There are doors to the "
    ++ (intercalate " and " (map (show . fst) doors))

tellItem :: (Item, String) -> IO ()
tellItem (item, pos) = tellContextLine $ pos ++ " there is a " ++ show item

tellMonster :: Monster -> IO ()
tellMonster monster = tellContextLine $ "There is a " ++ show monster

-- Main help for taking a game state and outputting information about it
tellContext :: GameState -> IO ()
tellContext (GS p r) = do
  putStrLn ""
  tellContextLine $ "You are in a " ++ name r ++ ". It is " ++ description r
  tellDoors (doors r)
  mapM tellItem    (items r)
  mapM tellMonster (monsters r)
  putStrLn ""
  return ()

opposite :: Direction -> Direction
opposite d | d == North = South
           | d == South = North
           | d == West  = East
           | otherwise  = West

noActions :: Item -> GameState -> Next GameState
noActions _ _ = Same "noAction"


startingRoom :: Room
startingRoom = Room { name        = "kitchen"
                    , description = "cold, dark"
                    , isWinRoom   = False
                    , requires    = Nothing
                    , items       = [(Spoon, "On a table")]
                    , monsters    = []
                    , doors       = [(East, winningRoom), (South, monsterRoom)]
                    , actions     = noActions
                    }


monsterRoom :: Room
monsterRoom = Room { name        = "garden"
                   , description = "weedy"
                   , isWinRoom   = False
                   , requires    = Nothing
                   , items       = []
                   , monsters    = [WoodTroll 10 Key]
                   , doors       = []
                   , actions     = noActions
                   }

winningRoom :: Room
winningRoom = Room { name        = "loot room"
                   , description = "shiny"
                   , isWinRoom   = True
                   , requires    = Just Key
                   , items       = []
                   , monsters    = []
                   , doors       = []
                   , actions     = noActions
                   }

game0 :: GameState
game0 = GS { player = Player "Dominic" [], room = startingRoom }

instance Parsable Item where
  parse str | "spoon" `isInfixOf` str = Just Spoon
            | "key" `isInfixOf` str   = Just Key
            | otherwise               = Nothing

instance Parsable Direction where
  parse str | "west" `isInfixOf` str  = Just West
            | "east" `isInfixOf` str  = Just East
            | "north" `isInfixOf` str = Just North
            | "south" `isInfixOf` str = Just South
            | otherwise               = Nothing

instance Parsable Command where
  parse str
    | "go" `isPrefixOf` str && isJust (parse str :: Maybe Direction) = Just
      (Move (fromJust (parse str :: Maybe Direction)))
    | "grab" `isPrefixOf` str && isJust (parse str :: Maybe Item) = Just
      (PickUp (fromJust (parse str :: Maybe Item)))
    | "use" `isPrefixOf` str && isJust (parse str :: Maybe Item) = Just
      (Use (fromJust (parse str :: Maybe Item)))
    | "end" `isPrefixOf` str = Just End
    | otherwise = Nothing

tellResponse :: String -> IO ()
tellResponse = putStrLn



areTheySame :: Eq a => a -> (a, b) -> [(a, b)]
areTheySame x (y, z) | x == y    = []
                     | otherwise = [(y, z)]

deleteFrom :: Eq a => a -> [(a, b)] -> [(a, b)]
deleteFrom _ []            = []
deleteFrom x ((y, z) : ys) = areTheySame x (y, z) ++ deleteFrom x ys

coutSame :: Eq a => a -> [(a, b)] -> Int
coutSame _ [] = 0
coutSame x ((y, z) : ys) =
  if null (areTheySame x (y, z)) then 1 else 0 + coutSame x ys

useItem :: Item -> GameState -> Next GameState
useItem Spoon gs
  | Spoon `notElem` inventory (player gs)
  = Progress "You do not have a spoon." gs
  | null (monsters (room gs))
  = Progress "Don't have monster" gs
  | health (head (monsters (room gs))) > 5
  = let pl = player gs
        r  = room gs
    in  Progress "You attack the monster but it seems only wounded."
                 GS { player = pl, room = r { monsters = [WoodTroll 5 Key] } }
  | otherwise
  = let pl = player gs
        r  = room gs
    in  Progress
          "You attack the monster with the spoon and it succumbs. It drops a key."
          GS { player = pl
             , room   = r { items = [(Key, "On the floor")], monsters = [] }
             }
useItem Key gs
  | Key `elem` inventory (player gs) = Progress "You do not have a key." gs
  | otherwise                        = Progress "Can't attack by key" gs

pickUpItem :: Item -> GameState -> Next GameState
pickUpItem Key gs
  | coutSame Key (items (room gs)) > 0
  = let pl = player gs
        r  = room gs
    in  Progress
          "You picked up the key."
          GS { player = pl, room = r { items = deleteFrom Key (items r) } }
  | otherwise
  = Progress "Don't have key in room." gs
pickUpItem Spoon gs
  | coutSame Spoon (items (room gs)) > 0
  = let pl = player gs
        r  = room gs
    in  Progress
          "You picked up the spoon."
          GS { player = pl, room = r { items = deleteFrom Spoon (items r) } }
  | otherwise
  = Progress "Don't have spoon in room." gs

moveDir :: Direction -> GameState -> Next GameState
moveDir dir gs =
  let door = lookup dir (doors (room gs))
      pl   = player gs
      r    = room gs
  in  if isNothing door
        then Progress "Can't go" gs
        else
          if isNothing (requires (fromJust door))
               ||     (fromJust (requires (fromJust door)))
               `elem` (inventory pl)
            then Progress
              ("You go through the door to the " ++ show dir ++ ".")
              GS { player = pl, room = leaveRoom r dir (fromJust door) }
            else Progress "This door requires a key." gs
step :: Command -> GameState -> Next GameState
step (Use    i  ) = useItem i
step (Move   dir) = moveDir dir
step (PickUp i  ) = pickUpItem i
step (End       ) = Progress "Stop game."

getItem :: Command -> Item
getItem (PickUp i) = i

readCommand :: GameState -> SCH.InputT IO ()
readCommand game = do
  minput <- SCH.getInputLine "> "
  let command = parse (fromJust minput) :: Maybe Command

  if isNothing command
    then do
      liftIO (putStrLn "Unknown command")
      readCommand game
    else do
      let Progress a b = step (fromJust command) game
      liftIO (putStrLn a)
      liftIO (tellContext b)
      let
        pl = player b
        r  = room b
        gs = GS
          { player = pl
            { inventory =
              inventory pl
                ++ ([ getItem (fromJust command) | "picked" `isInfixOf` a ])
            }
          , room   = r
          }
      if fromJust command == End
        then return ()
        else if isWinRoom r
          then liftIO (print ("You won " ++ playerName pl ++ "! Well done."))
          else readCommand gs


leaveRoom :: Room -> Direction -> Room -> Room
leaveRoom fromRoom dir toRoom =
  toRoom { doors = doors toRoom ++ [(opposite dir, fromRoom)] }

main :: IO ()
main = do
  tellContext game0
  SCH.runInputT SCH.defaultSettings $ readCommand game0

-- readCommand :: IO (Maybe Command)
