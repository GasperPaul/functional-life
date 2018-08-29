type State<'a> = State of 'a
type Position = int list
type Cell<'a> = State<'a> * Position
type Rule<'a> = State<'a> -> Cell<'a> list -> State<'a>
type World<'a> = World of Cell<'a> list
type Neighbourhood<'a> = World<'a> -> Position -> Cell<'a> list


let apply_rule world rule neighbourhood (state, position) =
  let neighbours = neighbourhood world position
  let state' = rule state neighbours
  (state', position)

let step_time (World world) rule neighbourhood =
  List.map (apply_rule (World world) rule neighbourhood) world 
  |> World

//--------------

type LifeStates = 
  | Alive
  | Dead

let rule_life (State state) neighbours =
  let isAlive ((State state), _) = 
    match state with
    | Alive -> true
    | _ -> false 

  let count =
    List.fold (fun a c -> a + if (c |> isAlive) then 1 else 0) 0 neighbours
    
  match state with
  | Alive when count < 2 || count > 3 -> Dead
  | Dead when count = 3 -> Alive 
  | state -> state
  |> State

let neighbourhood_life (World world) pos =
  let is_near c1 c2 = abs (c1-c2) <= 1

  world
  |> List.filter (fun (_,p) ->
      List.fold2 (fun r c1 c2 -> r && (is_near c1 c2)) true p pos && p <> pos
  )

//----------

let generate_world (str: System.String) =
  let generate_state = function
    | '#' -> Alive |> State
    | _ -> Dead |> State

  str.Split '\n'
  |> Seq.mapi (fun i line -> 
    line
    |> Seq.mapi (fun j c -> ((generate_state c), [i;j]))
  )
  |> Seq.collect id
  |> Seq.toList
  |> World

let initial_state = generate_world @"
0#00000000
00#0000000
###0000000
0000000000
0000000000
0000000000
0000000000
0000000000
0000000000
0000000000"  

let print_world (World world) = 
  let print_cell = function
    | Alive -> '#'
    | _ -> '0'

  let last ls = ls |> List.rev |> List.head
  let len = world |> List.map (fun (_,p) -> last p) |> List.max

  world
  |> List.fold (fun acc ((State s), p) -> 
    sprintf "%s%c%s" acc (print_cell s) (if (last p) = len then "\n" else "")
  ) ""
  |> printfn "%s"

let main =
  step_time initial_state rule_life neighbourhood_life
  |> print_world

main