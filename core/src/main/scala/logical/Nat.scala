package logical

sealed trait Nat {

  import Nat._

  def ===(that: Nat): Logic[Env, Unit] =
    (this, that) match {
      case (Zero, Zero) => Logic.succeed(())
      case (Zero, Succ(_)) => Logic.fail
      case (Succ(_), Zero) => Logic.fail
      case (Succ(x), Succ(y)) => x === y
    }

  def toInt: Logic[Env, Int] =
    this match {
      case Zero => Logic.succeed(0)
      case Succ(n) => n.get.flatMap(_.toInt).map(_ + 1)
    }

}

object Nat {

  private case object Zero extends Nat

  private case class Succ(n: Var[Nat]) extends Nat

  def apply(): Nat = Zero

  def apply(n: Var[Nat]): Nat = Succ(n)

  def apply(n: Int): Nat =
    if (n <= 0)
      Zero
    else
      Succ(apply(n - 1))

  def nat(n: Var[Nat]): Logic[Env, Unit] = {
    val m = Var[Nat]
    n === Zero ||| n === Succ(m) &&& nat(m)
  }

  def lteq(x: Var[Nat], y: Var[Nat]): Logic[Env, Unit] = {
    val n, m = Var[Nat]
    x === Zero ||| x === Succ(n) &&& y === Succ(m) &&& lteq(n, m)
  }

  def plus(x: Var[Nat], y: Var[Nat], z: Var[Nat]): Logic[Env, Unit] = {
    val n, m = Var[Nat]
    x === Zero &&& y === z ||| x === Succ(n) &&& z === Succ(m) &&& plus(n, y, m)
  }

  def divmod(x: Var[Nat], y: Var[Nat], q: Var[Nat], r: Var[Nat]): Logic[Env, Unit] = {
    val n, m = Var[Nat]
    lteq(Succ(x), y) &&& q === Zero &&& r === x ||| plus(n, y, x) &&& q === Succ(m) &&& divmod(n, y, m, r)
  }

  implicit val unify: Unify[Nat] =
    new Unify[Nat] {
      def unify(x: Nat, y: Nat): Logic[Env, Unit] = x === y
    }

}
