package logical

package fib

object Fib extends App {

  def fib(n: Var[Nat], m: Var[Nat]): Logic[Unit] = fib(n, Zero, Nat(1), m)

  def fib(a: Var[Nat], b: Var[Nat], c: Var[Nat], d: Var[Nat]): Logic[Unit] = {
    val n, m = Var[Nat]
    a === Zero &&& b === d ||| a === Succ(n) &&& Nat.plus(b, c, m) &&& fib(n, c, m, d)
  }

  val n = Var[Nat]
  (fib(Nat(args.head.toInt), n) &&& n.get.flatMap(_.toInt)).run.headOption.foreach(println)

}
