type State<A>         = A;
type Position         = Vec<i64>;
type Cell<A>          = (State<A>, Position);
type Rule<A>          = fn(&State<A>, Vec<&Cell<A>>) -> State<A>;
type World<A>         = Vec<Cell<A>>;
type Neighbourhood<A> = fn(&World<A>, Position) -> Vec<&Cell<A>>;

fn apply_rule<A>(world: &World<A>, rule: Rule<A>, neighbourhood: Neighbourhood<A>, cell: &Cell<A>) -> Cell<A> {
    let (state, position) = cell;
    let neighbours = neighbourhood(world, position.to_vec());
    let new_state = rule(state, neighbours);
    (new_state, position.to_vec())
}

fn step_time<A>(world: World<A>, rule: Rule<A>, neighbourhood: Neighbourhood<A>) -> World<A> {
    world.iter().map(|cell| apply_rule(&world, rule, neighbourhood, cell)).collect()
}

//--------------------

#[derive(Copy, Clone)]
enum LifeStates {
    Alive,
    Dead,
}

fn rule_life (state: &State<LifeStates>, neighbours: Vec<&Cell<LifeStates>>) -> State<LifeStates> {
    fn is_alive (cell: &Cell<LifeStates>) -> bool {
        if let (LifeStates::Alive, _) = cell { true } else { false }
    }

    let count = neighbours.iter().fold(0, |a,c| a + if is_alive(c) { 1 } else { 0 });
    match state {
        LifeStates::Alive if count < 2 || count > 3 => LifeStates::Dead,
        LifeStates::Dead if count == 3 => LifeStates::Alive,
        _ => state.clone(),
    }
}

fn neighbourhood_life (world: &World<LifeStates>, pos: Position) -> Vec<&Cell<LifeStates>> {
    fn is_near(c1: &i64, c2: &i64) -> bool { (c1-c2).abs() <= 1 }

    world.iter()
        .filter(|(_, p)| pos.iter().zip(p).fold(true, |r, (c1, c2)| r && is_near(c1, c2)) && p.iter().ne(pos.iter()))
        .collect()
}

//-----------------------------

fn generate_world(str: &str) -> World<LifeStates> {
    fn generate_state(ch: char) -> State<LifeStates> {
        if ch == '#' { LifeStates::Alive } else { LifeStates::Dead }
    }

    str.lines()
        .zip(0..)
        .flat_map(|(line, i)| line.chars().zip(0..).map(move |(c, j)| (generate_state(c), vec![i, j])))
        .collect::<World<LifeStates>>()
}

fn print_world(world: World<LifeStates>) {
    fn print_cell(state: &State<LifeStates>) -> &str {
        if let LifeStates::Alive = state { "#" } else { "0" }
    }

    if let Some((_, pos)) = world.iter().max_by_key(|(_, p)| p.iter().last()) { 
        let len = pos.last();
        let res = world.iter()
            .map(|(s, p)| print_cell(s).to_owned() + if p.last() == len { "\n" } else { "" })
            .collect::<Vec<String>>().join("");
        print!("{}", res);
    }
}

fn main() {
    let initial_state = generate_world ("
0#00000000
00#0000000
###0000000
0000000000
0000000000
0000000000
0000000000
0000000000
0000000000
0000000000");

    print_world(step_time(initial_state, rule_life, neighbourhood_life));
}
