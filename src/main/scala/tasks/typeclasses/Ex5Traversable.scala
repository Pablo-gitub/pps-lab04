package u04lab
import u03.Sequences.*
import Sequence.*
import u03.Optionals.Optional
import u03.Optionals.Optional.*

import scala.annotation.tailrec

/*  Exercise 5: 
 *  - Generalise by ad-hoc polymorphism logAll, such that:
 *  -- it can be called on Sequences but also on Optional, or others... 
 *  -- it does not necessarily call log, but any function with analogous type
 *  - Hint: introduce a type class Traversable[T[_]]], capturing the ability of calling a
 *    "consumer function" on all elements (with type A) of a datastructure T[A] 
 *    Note Traversable is a 2-kinded trait (similar to Filterable, or Monad)
 *  - Write givens for Traversable[Optional] and Traversable[Sequence]
 *  - Show you can use the generalisation of logAll to:
 *  -- log all elements of an Optional, or of a Traversable
 *  -- println(_) all elements of an Optional, or of a Traversable
 */

object Ex5Traversable:

  trait Traversable[T[_]]:
    def traverse[A](ta: T[A])(f: A => Unit): Unit

  given Traversable[Sequence] with
    def traverse[A](seq: Sequence[A])(f: A => Unit): Unit = seq match
      case Cons(h, t) =>
        f(h)
        traverse(t)(f)
      case Nil() => ()

  given Traversable[Optional] with
    def traverse[A](opt: Optional[A])(f: A => Unit): Unit = opt match
      case Just(a) => f(a)
      case Empty() => ()

  def log[A](a: A): Unit = println("The next element is: "+a)

  @tailrec
  def logAll[A](seq: Sequence[A]): Unit = seq match
    case Cons(h, t) => log(h); logAll(t)
    case _ => ()

  def logAllGen[T[_] : Traversable, A](ta: T[A])(log: A => Unit): Unit =
    summon[Traversable[T]].traverse(ta)(log)

  def printAll[T[_] : Traversable, A](ta: T[A]): Unit =
    logAllGen(ta)(println)

@main def tryLogAll(): Unit =

  import Ex5Traversable.*
  import u03.Optionals.Optional.*

  // Test with Sequence:
  val seq: Sequence[Int] = Cons(10, Cons(20, Cons(30, Nil())))
  println("LogAllGen on Sequence using log:")
  logAllGen(seq)(log)
  println("PrintAll on Sequence using println:")
  printAll(seq)

  // Test with Optional:
  val opt1: Optional[String] = Just("Hello")
  val opt2: Optional[String] = Empty()

  println("LogAllGen on Optional (Just):")
  logAllGen(opt1)(log)
  println("PrintAll on Optional (Just):")
  printAll(opt1)

  println("LogAllGen on Optional (Empty):")
  logAllGen(opt2)(log)
  println("PrintAll on Optional (Empty):")
  printAll(opt2)