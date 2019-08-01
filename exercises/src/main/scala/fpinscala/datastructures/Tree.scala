package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_)      => 1
    case Branch(l, r) => size(l) + size(r) + 1
  }

  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(x)      => x
    case Branch(l, r) => maximum(l) max maximum(r)
  }

  def depth[A](t: Tree[A]): Int = {
    def loop[A](t: Tree[A], depth: Int): Int =
      t match {
        case Leaf(_)      => depth
        case Branch(l, r) => loop(l, depth+1) max loop(r, depth+1)
      }

    loop(t, 0)
  }

  def map[A,B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(x)      => Leaf(f(x))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  // size, maximum, depth, map
  def fold[A,B](t: Tree[A])(l: A => B)(b: (B, B) => B): B =
    t match {
      case Leaf(a) => l(a)
      case Branch(left, right) => b(fold(left)(l)(b), fold(right)(l)(b))
    }

  def size2[A](t: Tree[A]): Int =
    fold(t)(a => 1)((l, r) => l + r + 1)

}

object TreeTest extends App {

  import Tree._

  val t1 = Branch(Leaf(1), Branch(Branch(Leaf(2), Leaf(3)), Leaf(4)))
  println(size(t1))
  println(size2(t1))
}
