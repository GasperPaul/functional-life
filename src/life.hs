data State a = State a
type Position = [Int]
type Cell a = (State a, Position)
type Rule a = State a -> [Cell a] -> State a
data World a = World [Cell a]
type Neighbourhood a = World a -> Position -> [Cell a]

apply_rule :: World a -> Rule a -> Neighbourhood a -> Cell a -> Cell a
apply_rule world rule neighbourhood (state, position) = (state', position)
  where neighbours = neighbourhood world position
        state' = rule state neighbours

step_time :: World a -> Rule a -> Neighbourhood a -> World a
step_time (World world) rule neighbourhood =
  World $ map (apply_rule (World world) rule neighbourhood) world
  
-------------------

data LifeStates = Alive | Dead deriving (Eq)

rule_life :: Rule LifeStates
rule_life (State state) neighbours
  | (state == Alive) && (count < 2) || (count > 3) = State Dead
  | (state == Dead) && (count == 3) = State Alive
  | otherwise = State state
  where
    isAlive ((State state), _) 
      | state == Alive = True
      | otherwise = False
    count = foldl (\a c -> a + if (isAlive c) then 1 else 0) 0 neighbours

neighbourhood_life :: Neighbourhood LifeStates
neighbourhood_life (World world) pos =
  filter filter_neighbours world
  where
    is_near c1 c2 = abs(c1-c2) <= 1
    is_neighbour r (c1,c2) = r && is_near c1 c2
    filter_neighbours (_,p) = (foldl is_neighbour True $ zip p pos) && (p /= pos)
    
-------------------

generate_world :: String -> World LifeStates
generate_world str = 
  World $ concat $ map for_string $ zip [0..] $ lines str
  where
    generate_state ch = if (ch == '#') then State Alive else State Dead
    for_string (i, line) = map (\(j, c) -> (generate_state c, [i, j])) $ zip [0..] line

initial_state :: World LifeStates
initial_state = generate_world "0#00000000\n00#0000000\n###0000000\n0000000000\n0000000000\n0000000000\n0000000000\n0000000000\n0000000000\n0000000000"

print_world :: World LifeStates -> IO()
print_world (World world) = 
  putStrLn $ concat $ fmap to_str world
  where
    print_cell state
      | state == Alive = "#"
      | otherwise = "0"
    len = maximum $ map (\(_,p) -> last p) world
    to_str ((State s), p) = (print_cell s) ++ if last p == len then "\n" else ""

main :: IO()
main =
  print_world $ step_time initial_state rule_life neighbourhood_life