package logical

import scala.annotation.tailrec

sealed abstract class Arrs[-A, +B] {
  def :+[C](f: B => Logic[C]): Arrs[A, C] = Arrs.Node(this, Arrs.Leaf(f))

  def ++[C](that: Arrs[B, C]): Arrs[A, C] = Arrs.Node(this, that)

  def apply(a: A): Logic[B] = {
    @tailrec def loop[A](a: A, arrs: Arrs[A, B]): Logic[B] =
      arrs match {
        case Arrs.Leaf(f) => f(a)
        case Arrs.Node(l, r) =>
          l match {
            case Arrs.Leaf(f) =>
              f(a) match {
                case Logic.Success(v) => loop(v, r)
                case Logic.Failure => Logic.Failure
                case Logic.FlatMap(effect, arrs) => Logic.FlatMap(effect, arrs ++ r)
              }
            case Arrs.Node(ll, lr) =>
              loop(a, Arrs.Node(ll, Arrs.Node(lr, r)))
          }
      }
    loop(a, this)
  }
}

object Arrs {
  def apply[A, B](f: A => Logic[B]): Arrs[A, B] = Leaf(f)

  case class Leaf[A, B](f: A => Logic[B]) extends Arrs[A, B]

  case class Node[A, B, C](left: Arrs[A, B], right: Arrs[B, C]) extends Arrs[A, C]
}
