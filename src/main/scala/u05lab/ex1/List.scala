package u05lab.ex1

import u05lab.ex1.List

import scala.annotation.tailrec

// Ex 1. implement the missing methods both with recursion or with using fold, map, flatMap, and filters
// List as a pure interface
enum List[A]:
  case ::(h: A, t: List[A])
  case Nil()
  def ::(h: A): List[A] = List.::(h, this)

  def head: Option[A] = this match
    case h :: t => Some(h)
    case _ => None

  def tail: Option[List[A]] = this match
    case h :: t => Some(t)
    case _ => None

  def append(list: List[A]): List[A] = this match
    case h :: t => h :: t.append(list)
    case _ => list

  def foreach(consumer: A => Unit): Unit = this match
    case h :: t => consumer(h); t.foreach(consumer)
    case _ =>

  def get(pos: Int): Option[A] = this match
    case h :: t if pos == 0 => Some(h)
    case h :: t if pos > 0 => t.get(pos - 1)
    case _ => None

  def filter(predicate: A => Boolean): List[A] = this match
    case h :: t if predicate(h) => h :: t.filter(predicate)
    case _ :: t => t.filter(predicate)
    case _ => Nil()

  def map[B](fun: A => B): List[B] = this match
    case h :: t => fun(h) :: t.map(fun)
    case _ => Nil()

  def flatMap[B](f: A => List[B]): List[B] =
    foldRight[List[B]](Nil())(f(_) append _)

  def foldLeft[B](z: B)(op: (B, A) => B): B = this match
    case h :: t => t.foldLeft(op(z, h))(op)
    case Nil() => z

  def foldRight[B](z: B)(f: (A, B) => B): B = this match
    case h :: t => f(h, t.foldRight(z)(f))
    case _ => z

  def length: Int = foldLeft(0)((l, _) => l + 1)

  def isEmpty: Boolean = this match
    case Nil() => true
    case _ => false

  def reverse(): List[A] = foldLeft[List[A]](Nil())((l, e) => e :: l)

  /** EXERCISES */
  def zipRight : List[(A, Int)] = this.reverse().foldRight(Nil())((a, b) => b.append(List((a, b.length))))


  /*def zipRight: List[(A, Int)] =
    def f(a: A, i: Int) : (A, Int) = (a, i)
    this match
      case Nil() => Nil()*/
      //case h :: t => t.foldRight[List[A]](Nil())((l, e) => (f(l, 0) :: Nil()))

  /*def zipRight: List[(A, Int)] =
    @tailrec
    def zipRight2(l: List[A], i: Int) : List[(A, Int)] = l match
      case Nil() => Nil()
      case h :: t => zipRight2((h, i) :: Nil(), i + 1)
    zipRight2(this, 0)*/
    /*@tailrec
    def max(l: List[Int], acc: Option[Int]): Option[Int] = (l, acc) match
      case (Cons(h, t), None()) => max(t, Some(h))
      case (Cons(h, t), acc) if h > orElse(acc, h) => max(t, Some(h))
      case (Cons(_, Nil()), _) | (Nil(), _) => acc
      case (Cons(_, t), acc) => max(t, acc)

    max(l, None())*/



  def partition(pred: A => Boolean): (List[A], List[A]) = ???

  def span(pred: A => Boolean): (List[A], List[A]) = ???

  /** @throws UnsupportedOperationException if the list is empty */
  def reduce(op: (A, A) => A): A = ???

  def takeRight(n: Int): List[A] = ???

// Factories
object List:

  def apply[A](elems: A*): List[A] =
    var list: List[A] = Nil()
    for e <- elems.reverse do list = e :: list
    list

  def of[A](elem: A, n: Int): List[A] =
    if n == 0 then Nil() else elem :: of(elem, n - 1)

@main def checkBehaviour(): Unit =
  val reference = List(1, 2, 3, 4)
  println(reference.zipRight) // List((1, 0), (2, 1), (3, 2), (4, 3))
  println(reference.partition(_ % 2 == 0)) // (List(2, 4), List(1, 3))
  println(reference.span(_ % 2 != 0)) // (List(1), List(2, 3, 4))
  println(reference.span(_ < 3)) // (List(1, 2), List(3, 4))
  println(reference.reduce(_ + _)) // 10
  try Nil.reduce[Int](_ + _)
  catch case ex: Exception => println(ex) // prints exception
  println(List(10).reduce(_ + _)) // 10
  println(reference.takeRight(3)) // List(2, 3, 4)
