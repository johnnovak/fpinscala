package fpinscala.laziness

import Stream._

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)
  def from(n: Int): Stream[Int] = ???

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = ???
}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

trait Stream[+A] {

  def toList: List[A] = {
    @annotation.tailrec
    def loop(s: Stream[A], acc: List[A]): List[A] = s match {
      case Cons(h, t) => loop(t(), h() :: acc)
      case Empty => acc
    }
    loop(this, Nil).reverse
  }

  def take(n: Int): Stream[A] = {
    def loop(s: Stream[A], n: Int): Stream[A] = s match {
      case Cons(h, t) if n > 0 => cons(h(), loop(t(), n-1))
      case _ => empty
    }
    loop(this, n)
  }

  @annotation.tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => t().drop(n-1)
    case _ => this
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => empty
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  def headOption: Option[A] = ???


  def map[B](f: A => B): Stream[B] = ???

  def filter(f: A => Boolean): Stream[A] = ???

  def append[B >: A](b: B): Stream[B] = ???

  def flatMap[B](f: A => Option[B]): Stream[B] = ???

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def startsWith[B](s: Stream[B]): Boolean = ???
}


object StreamTest extends App {
  val s = Stream(1,2,3,4,5)
  println(s.takeWhile(_ <= 3).toList)
}
