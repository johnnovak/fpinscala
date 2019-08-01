package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] =
    l match {
      case Nil => Nil
      case Cons(h, t) => t
    }

  def setHead[A](l: List[A], h: A): List[A] =
    l match {
      case Nil => List(h)
      case Cons(_, t) => Cons(h, t)
    }

  def drop[A](l: List[A], n: Int): List[A] =
    if (n <= 0) l
    else l match {
      case Nil => Nil
      case Cons(h, t) => drop(t, n-1)
    }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match {
      case Nil => Nil
      case Cons(h, t) =>
        if (f(h)) dropWhile(t, f)
        else l
    }

  def init[A](l: List[A]): List[A] =
    l match {
      case Nil => sys.error("empty list")
      case Cons(_, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
    }

  def length[A](l: List[A]): Int =
    foldRight(l, 0)((_, z) => z + 1)

  @annotation.tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(h, t) => foldLeft(t, f(z,h))(f)
  }

  def sum3(l: List[Int]): Int = foldLeft(l, 0)(_ + _)
  def product3(l: List[Int]): Int = foldLeft(l, 1)(_ * _)

  def length2[A](l: List[A]): Int =
    foldLeft(l, 0)((z, _) => z + 1)

  def append2[A](a1: List[A], a2: List[A]): List[A] =
    foldRight(a1, a2)(Cons(_, _))

  def concat[A](ll: List[List[A]]): List[A] =
    foldRight(ll, Nil:List[A])(append2)

  def add1(l: List[Int]): List[Int] =
    foldRight(l, Nil:List[Int])((h, t) => Cons(h+1, t))

  def doubleToString(l: List[Double]): List[String] =
    foldRight(l, Nil:List[String])((h, t) => Cons(h.toString, t))

  def map[A,B](l: List[A])(f: A => B): List[B] =
    foldRight(l, Nil:List[B])((h, t) => Cons(f(h), t))

  def filter[A](l: List[A])(f: A => Boolean): List[A] =
    foldRight(l, Nil:List[A])((h, t) => if (f(h)) Cons(h, t) else t)

  def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] =
    concat(map(l)(f))

  def filterViaFlatMap[A](l: List[A])(f: A => Boolean): List[A] =
    flatMap(l)(a => if (f(a)) List(a) else Nil)

  def pairwiseAdd(a1: List[Int], a2: List[Int]): List[Int] =
    (a1, a2) match {
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, pairwiseAdd(t1, t2))
      case (Cons(h1, t1), Nil)          => Cons(h1, pairwiseAdd(t1, Nil))
      case (Nil,          Cons(h2, t2)) => Cons(h2, pairwiseAdd(Nil, t2))
      case (Nil,          Nil)          => Nil
    }

  def pairwiseAdd2(a1: List[Int], a2: List[Int]): List[Int] =
    (a1, a2) match {
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, pairwiseAdd2(t1, t2))
      case _ => Nil
    }

  def zipWith[A,B,C](as: List[A], bs: List[B])(f: (A, B) => C): List[C] =
    (as, bs) match {
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
      case _ => Nil
    }

  @annotation.tailrec
  def startsWith[A](l: List[A], prefix: List[A]): Boolean =
    (l, prefix) match {
      case (_, Nil) => true
      case (Cons(lh, lt), Cons(ph, pt)) if (lh == ph) => startsWith(lt, pt)
      case _ => false
    }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    @annotation.tailrec
    def loop[A](l: List[A], found: Boolean): Boolean =
      (l, found) match {
        case (_,          true)  => true
        case (Nil,        false) => false
        case (Cons(h, t), false) => loop(t, startsWith(l, sub))
      }

    loop(sup, false)
  }


}

object ListTest extends App {

  import List._

  println( hasSubsequence(List(1,2), List(1,2)) )
  println( hasSubsequence(List(1,2,3,4,5), List(3,4)) )
  println( hasSubsequence(List(1,2,3,4,5), List(4)) )
  println( hasSubsequence(List(1,2,3,4,5), Nil) )
  println()
  println( hasSubsequence(List(1,2,3,4,5), List(6,7,8)) )
  println( hasSubsequence(List(1,2), List(6,7,8)) )
  println( hasSubsequence(List(1,2), List(9)) )
}
