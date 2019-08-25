package fpinscala.testing

import fpinscala.laziness.Stream
import fpinscala.state._
import fpinscala.parallelism._
import fpinscala.parallelism.Par.Par
import Gen._
import Prop._
import java.util.concurrent.{Executors,ExecutorService}

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/

trait Prop {
  def check: Either[(FailedCase, SuccessCount), SuccessCount]
//  def &&(p: Prop): Prop = new Prop {
//    def check = this.check && p.check
//  }
  def &&(p: Prop): Prop = ???
}

object Prop {
  type FailedCase = String
  type SuccessCount = Int

  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = ???
}

case class Gen[A](sample: State[RNG, A]) {

  def flatMap[B](f: A => Gen[B]): Gen[B] =
 //   Gen(sample.map(f).flatMap(_.sample))
    Gen(sample.flatMap(a => f(a).sample))

  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size.flatMap(n => Gen.listOfN(n, this))

}

object Gen {
  def unit[A](a: => A): Gen[A] = Gen(State.unit(a))

  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State(RNG.nonNegativeLessThan(stopExclusive)).map(_ + start))

  def boolean: Gen[Boolean] =
    Gen(choose(0, 2).sample.map(_ == 1))

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(g.sample)))

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    choose(0, 2).flatMap(r => if (r == 0) g1 else g2)

  def double: Gen[Double] =
    Gen(State(RNG.double))

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] =
    double.flatMap(d => if (d * (g1._2 + g2._2) < g1._2) g1._1 else g2._1)
}

/*
trait Gen[A] {
  def map[A,B](f: A => B): Gen[B] = ???
  def flatMap[A,B](f: A => Gen[B]): Gen[B] = ???
}
*/

trait SGen[+A] {

}

