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
      Succ(Var(apply(n - 1)))

  def nat(n: Var[Nat]): Logic[Env, Unit] = {
    val m = Var[Nat]
    n === Var(Zero) ||| n === Var(Succ(m)) &&& nat(m)
  }

  def lteq(x: Var[Nat], y: Var[Nat]): Logic[Env, Unit] = {
    val px = Var[Nat]
    val py = Var[Nat]
    x === Var(Zero) ||| x === Var(Succ(px)) &&& y === Var(Succ(py)) &&& lteq(px, py)
  }

  def plus(x: Var[Nat], y: Var[Nat], z: Var[Nat]): Logic[Env, Unit] = {
    val px = Var[Nat]
    val pz = Var[Nat]
    x === Var(Zero) &&& y === z ||| x === Var(Succ(px)) &&& z === Var(Succ(pz)) &&& plus(px, y, pz)
  }

  def divmod(x: Var[Nat], y: Var[Nat], q: Var[Nat], r: Var[Nat]): Logic[Env, Unit] = {
    val z = Var[Nat]
    val pq = Var[Nat]
    lteq(Var(Succ(x)), y) &&& q === Var(Zero) &&& r === x ||| plus(z, y, x) &&& q === Var(Succ(pq)) &&& divmod(z, y, pq, r)
  }

  implicit val unify: Unify[Nat] =
    new Unify[Nat] {
      def unify(x: Nat, y: Nat): Logic[Env, Unit] = x === y
    }

}
