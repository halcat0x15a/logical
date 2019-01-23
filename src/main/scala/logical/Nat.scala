package logical

import scala.annotation.tailrec

sealed abstract class Nat {
  final def toInt: Logic[Int] =
    this match {
      case Nat.Z => Logic.Success(0)
      case Nat.S(n) => n.toInt.map(_ + 1)
    }
}

object Nat {
  case object Z extends Nat

  case class S(n: LVar[Nat]) extends Nat

  def apply(n: Int): Nat = {
    @tailrec def loop(m: Int, nat: Nat): Nat =
      if (n == m) nat else loop(m + 1, S(nat))
    if (n <= 0) Z else loop(0, Z)
  }

  def plus(x: LVar[Nat], y: LVar[Nat], z: LVar[Nat]): Logic[Unit] =
    x === Z &&& y === z ||| (for {
      m <- LVar[Nat]
      n <- LVar[Nat]
      _ <- x === S(m) &&& z === S(n) &&& plus(m, y, n)
    } yield ())

  def divmod(x: LVar[Nat], y: LVar[Nat], q: LVar[Nat], r: LVar[Nat]): Logic[Unit] =
    x < y &&& q === Z &&& r === x ||| (for {
      m <- LVar[Nat]
      n <- LVar[Nat]
      _ <- plus(m, y, x) &&& q === S(n) &&& divmod(m, y, n, r)
    } yield ())

  implicit class NatOps(val self: Nat) extends AnyVal {
    def ===(that: Nat): Logic[Unit] =
      (self, that) match {
        case (Z, Z) =>
          Logic.unit
        case (S(m), S(n)) =>
          m === n
        case _ =>
          Logic.Failure
      }
  }

  implicit class LVarOps(val self: LVar[Nat]) extends AnyVal {
    def <=(that: LVar[Nat]): Logic[Unit] =
      self === Z ||| (for {
        m <- LVar[Nat]
        n <- LVar[Nat]
        _ <- self === S(m) &&& that === S(n) &&& m <= n
      } yield ())

    def <(that: LVar[Nat]): Logic[Unit] = that >= S(self)

    def >=(that: LVar[Nat]): Logic[Unit] = that <= self

    def >(that: LVar[Nat]): Logic[Unit] = self >= S(that)

    def toInt: Logic[Int] = self.get.flatMap(_.toInt)
  }

  implicit val unify: Unify[Nat] =
    new Unify[Nat] {
      def apply(m: Nat, n: Nat): Logic[Unit] = m === n
    }
}
