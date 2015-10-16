/*package logical

sealed abstract class Nat {

  def ===(that: Nat): Logic[Unit] =
    (this, that) match {
      case (Zero, Zero) => Logic.True
      case (Zero, Succ(_)) => Failure
      case (Succ(_), Zero) => Failure
      case (Succ(x), Succ(y)) => x === y
    }

  def toInt: Logic[Int] =
    this match {
      case Zero => Logic.success(0)
      case Succ(n) => n.get.flatMap(_.toInt).map(_ + 1)
    }

}

case object Zero extends Nat

case class Succ(n: Var[Nat]) extends Nat

object Nat {

  val numbers: Stream[Nat] = Stream.iterate(Zero: Nat)(Succ(_))

  def apply(n: Int): Nat =
    if (n <= 0)
      Zero
    else
      numbers(n)

  def nat(n: Var[Nat]): Logic[Unit] = {
    val m = Var[Nat]
    n === Zero ||| n === Succ(m) &&& nat(m)
  }

  def lteq(x: Var[Nat], y: Var[Nat]): Logic[Unit] = {
    val n, m = Var[Nat]
    x === Zero ||| x === Succ(n) &&& y === Succ(m) &&& lteq(n, m)
  }

  def plus(x: Var[Nat], y: Var[Nat], z: Var[Nat]): Logic[Unit] = {
    val n, m = Var[Nat]
    x === Zero &&& y === z ||| x === Succ(n) &&& z === Succ(m) &&& plus(n, y, m)
  }

  def divmod(x: Var[Nat], y: Var[Nat], q: Var[Nat], r: Var[Nat]): Logic[Unit] = {
    val n, m = Var[Nat]
    lteq(Succ(x), y) &&& q === Zero &&& r === x ||| plus(n, y, x) &&& q === Succ(m) &&& divmod(n, y, m, r)
  }

  implicit val unify: Unify[Nat] =
    new Unify[Nat] {
      def unify(x: Nat, y: Nat): Logic[Unit] = x === y
    }

}
 */
