import scala.io.Source

// 1. Introduction
val boxsize: Int = 3
val boardsize: Int = boxsize*boxsize 
val cellvals: List[Char] = "123456789".toList
val blank: Char => Boolean = _ == '.'

// 2. Specification

type Matrix[+A] = List[List[A]]
type Board = Matrix[Char]

def toString[A](b: Matrix[A]) = b.grouped(boxsize).map(
    _.map(
        _.grouped(boxsize).map(_.mkString(" ")).mkString("  ")
    ).mkString("\n")
).mkString("\n\n")

// 2.1 Rows, columns and boxes
def group[A](m: List[A]): List[List[A]] = m.grouped(boxsize).toList 
def ungroup[A](m: List[List[A]]): List[A] = m.flatten 

def rows[A](m: Matrix[A]): Matrix[A] = m 
def cols[A](m: Matrix[A]): Matrix[A] = m.transpose
def boxes[A](m: Matrix[A]): Matrix[A] = 
    ungroup(group(m.map(group(_))).map(cols(_))).map(ungroup(_)).toList

def nodups[A](l: List[A]): Boolean = l match
    case Nil => true 
    case hd :: tl => !tl.contains(hd) && nodups(tl)

def correct(b: Board): Boolean = 
    rows(b).map(nodups(_)).forall(identity) && 
    cols(b).map(nodups(_)).forall(identity) && 
    boxes(b).map(nodups(_)).forall(identity)

// 2.2 Generating choices and matrix cartesian product
// At this point, we start working with iterators, because the original paper
// is in Haskell which has lazy evaluation while scala does not (actually, it does but not very easy here)

type Choices = List[Char]

def choose(e: Char) = if blank(e) then cellvals else e :: Nil 
def choices(b: Board): Matrix[Choices] = b.map(_.map(choose))

def cp[A](lst: List[List[A]]): Iterator[List[A]] = lst match
    case hd :: tl => for {
        x <- hd.iterator
        y <- cp(tl)
    } yield x :: y
    case Nil => Iterator(Nil)

// slightly different implementation
// not actually used, but helpful for understanding what's going on
def mcp[A](m: Matrix[List[A]]): Iterator[Matrix[A]] = cp(m.flatten).map(_.grouped(boardsize).toList)

// 3. Pruning the choices

def fixed(l: List[Choices]): Choices = l.filter(_.size == 1).flatten

def remove(fs: Choices, cs: Choices): Choices = if cs.size == 1 then cs else cs.filter(c => fs.map(_ != c).forall(identity))

def reduce(css: List[Choices]): List[Choices] = css.map(cs => remove(fixed(css), cs))

def pruneBy(f: Matrix[Choices] => Matrix[Choices]): Matrix[Choices] => Matrix[Choices] = m => f(f(m).map(reduce))

def prune(mcs: Matrix[Choices]): Matrix[Choices] = pruneBy(boxes)(pruneBy(rows)(pruneBy(cols)(mcs)))

// 4.1 Blocked matrices
def void(cm: Matrix[Choices]) = !cm.flatten.map(_.size != 0).forall(identity)

def safe(cm: Matrix[Choices]) = rows(cm).forall(a => nodups(fixed(a))) && cols(cm).forall(a => nodups(fixed(a))) && boxes(cm).forall(a => nodups(fixed(a)))

def blocked(cm: Matrix[Choices]): Boolean = void(cm) || !safe(cm)

// 4.2 Smallest number of choices
def search(rows1: Matrix[Choices], row1: List[Choices], c: Choices, row2: List[Choices], rows2: Matrix[Choices], param: Int): List[Matrix[Choices]] = if c.size == param then 
    for {
        cs <- c
    } yield rows1 ::: List(row1 ::: (List(cs) :: row2)) ::: rows2
    else 
    (rows1, row1, c, row2, rows2) match 
    case (rows1, row1, c, row2_hd :: row2_tl, rows2) => search(rows1, row1 ::: List(c), row2_hd, row2_tl, rows2, param)
    case (rows1, row1, c, Nil, (hd :: tl1) :: tl2) => search(rows1 ::: List(row1 ::: List(c)), Nil, hd, tl1, tl2, param)
    case _ => throw new Exception("Should never get here")

def expand(cm: Matrix[Choices]): List[Matrix[Choices]] = cm match
    case (hd :: tl1) :: tl2 => search(Nil, Nil, hd, tl1, tl2, cm.flatten.map(_.size).min)
    case _ => throw new Exception("Should never get here")

def search(cm: Matrix[Choices]): List[Matrix[Choices]] = 
    if blocked(cm) then Nil 
    else if cm.flatten.forall(_.size == 1) then cm :: Nil 
    else expand(cm).map(a => search(prune(a))).flatten

// 4.3 Final version
// mistake in paper (should map head through 3 maps not just 2)
def sudoku(b: Board): List[Board] = search(prune(choices(b))).map(_.map(_.map(_.head)))

@main 
def run(puzzleFile: String, puzzleIndex: Int) = 
    val board = Source.fromFile(puzzleFile).getLines.drop(puzzleIndex-1).next.grouped(boardsize).map(_.toList).toList

    println("Unsolved board: ")

    println(toString(board))

    println("\nSolving board...\n");

    val res = sudoku(board) 

    if res.size == 0 then 
        println("No solutions!")
    else 
        println(s"Found ${res.size} solution${if res.size == 1 then "" else "s"}!")

        for {
            sol <- res 
        }
            println(toString(sol))
        
    
