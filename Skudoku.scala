
// 1. Introduction
val boxsize: Int = 2
val boardsize: Int = boxsize*boxsize 
val cellvals: String = "123456789"
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

@main 
def run = 
    val b: Board = List(
        List('1', '2', '1', '2'),
        List('3', '4', '.', '.'),
        List('2', '3', '.', '.'),
        List('2', '3', '.', '.'),
    )
    println(toString(boxes(b)))
    // println("Running! Sick cooler")


// tests (to write)
// 1. test row composed with row is identity 