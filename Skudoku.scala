
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


@main 
def run = 
    val b: Board = List(
        List('.', '.', '3', '.', '.', '.', '2', '.', '.'),
        List('.', '6', '.', '9', '8', '.', '.', '4', '3'),
        List('4', '9', '.', '.', '3', '1', '.', '.', '6'),
        List('9', '.', '7', '.', '.', '.', '8', '6', '.'),
        List('.', '4', '.', '.', '9', '8', '.', '.', '.'),
        List('.', '.', '5', '4', '.', '7', '1', '.', '9'),
        List('6', '.', '.', '.', '.', '3', '9', '.', '5'),
        List('5', '.', '8', '1', '.', '.', '.', '7', '2'),
        List('2', '.', '9', '.', '5', '6', '.', '3', '8')
    )
    println(prune(choices(b)))
