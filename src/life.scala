type State[A] = A
type Position = List[Int]
type Cell[A] = (State[A], Position)
type Rule[A] = (State[A], List[Cell[A]]) => (State[A])
type World[A] = List[Cell[A]]
type Neighbourhood[A] = (World[A], Position) => (List[Cell[A]])

def apply_rule[A] (world: World[A], rule: Rule[A], neighbourhood:Neighbourhood[A]) (cell:Cell[A]) = {
  val (state, position) = cell
  val neighbours = neighbourhood (world, position)
  val newState = rule (state, neighbours)
  (newState, position)
}

def step_time[A] (world:World[A], rule:Rule[A], neighbourhood:Neighbourhood[A]) =
  world.map(apply_rule (world, rule, neighbourhood))

//--------------

abstract class LifeStates
case object Alive extends LifeStates
case object Dead extends LifeStates

def rule_life (state: State[LifeStates], neighbours: List[Cell[LifeStates]]) = {
  def isAlive (cell: Cell[LifeStates]) = cell match {
    case (Alive, _) => true
    case _ => false
  }

  val count = 
		neighbours.foldLeft(0) { (a,c) => a + (if (isAlive (c)) 1 else 0) }

  state match {
		case Alive if count < 2 || count > 3 => Dead
    case Dead if count == 3 => Alive
    case other => other
  }
}

def neighbourhood_life (world: World[LifeStates], pos: Position) = {
  def is_near (c1: Int, c2: Int): Boolean = math.abs(c1 - c2) <= 1
  world.filter { case (_,p) =>
      (pos.zip(p).foldLeft(true) { (r, el) => {
          val (c1,c2) = el
          r && (is_near(c1,c2)) 
      	}
      }) && (p != pos)
  }
}

//-------------

def generate_world (str: String) = {
	def generate_state (ch: Char) = ch match {
  	case '#' => Alive
    case _ => Dead
  }
  
  str.split('\n').zipWithIndex.flatMap { case (line, i) =>
  	line.zipWithIndex.map { case (c, j) => (generate_state(c), List(i, j)) }
  }.toList
}

val initial_state = generate_world ("""
0#00000000
00#0000000
###0000000
0000000000
0000000000
0000000000
0000000000
0000000000
0000000000
0000000000""")

def print_world (world: World[LifeStates]) = {
  def print_cell (state: State[LifeStates]) = state match {
		case Alive => '#'
    case Dead => '0'
  }
  
  val len = world.map { case (_, p) => p.last }.max
  val res = world.foldLeft("") { case (acc, (s, p)) => acc + (print_cell (s)) + (if (p.last == len) '\n' else "") }
	println (res)
}

def main () =
	print_world (step_time (initial_state, rule_life, neighbourhood_life))

main ()