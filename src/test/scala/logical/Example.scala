package logical

import org.scalatest.FunSuite

class Example extends FunSuite {

  test("Cons.append") {
    val xs = Var[Cons[Int]]
    val ys = Var[Cons[Int]]
    assert((Cons.append(Var(Cons(1, 2, 3)), Var(Cons(4, 5)), xs) &&& xs.get.flatMap(_.toList)).run == Stream(List(1, 2, 3, 4, 5)))
    assert((Cons.append(Var(Cons(1, 2, 3)), xs, Var(Cons(1, 2, 3, 4, 5))) &&& xs.get.flatMap(_.toList)).run == Stream(List(4, 5)))
    assert((Cons.append(xs, ys, Var(Cons(1, 2, 3))) &&& (for (xs <- xs.get.flatMap(_.toList); ys <- ys.get.flatMap(_.toList)) yield (xs, ys))).run == Stream((Nil, List(1, 2, 3)), (List(1), List(2, 3)), (List(1, 2), List(3)), (List(1, 2, 3), Nil)))
  }

  test("Nat.plus") {
    val n = Var[Nat]
    val m = Var[Nat]
    assert((Nat.plus(Var(Nat(2)), Var(Nat(3)), n) &&& n.get.flatMap(_.toInt)).run == Stream(5))
    assert((Nat.plus(Var(Nat(2)), n, Var(Nat(5))) &&& n.get.flatMap(_.toInt)).run == Stream(3))
    assert((Nat.plus(n, m, Var(Nat(3))) &&& (for (n <- n.get.flatMap(_.toInt); m <- m.get.flatMap(_.toInt)) yield (n, m))).run == Stream((0, 3), (1, 2), (2, 1), (3, 0)))
  }

  test("Nat.lteq") {
    val n = Var[Nat]
    assert(Nat.lteq(Var(Nat(2)), Var(Nat(3))).run == Stream(()))
    assert(Nat.lteq(Var(Nat(3)), Var(Nat(3))).run == Stream(()))
    assert(Nat.lteq(Var(Nat(4)), Var(Nat(3))).run == Stream())
    assert((Nat.lteq(n, Var(Nat(3))) &&& n.get.flatMap(_.toInt)).run == Stream(0, 1, 2, 3))
  }

  test("Nat.lt") {
    val n = Var[Nat]
    assert(Nat.lt(Var(Nat(2)), Var(Nat(3))).run == Stream(()))
    assert(Nat.lt(Var(Nat(3)), Var(Nat(3))).run == Stream())
    assert((Nat.lt(n, Var(Nat(3))) &&& n.get.flatMap(_.toInt)).run == Stream(0, 1, 2))
  }

  test("Nat.mod") {
    val q = Var[Nat]
    val r = Var[Nat]
    assert((Nat.divmod(Var(Nat(1)), Var(Nat(2)), q, r) &&& (for (q <- q.get.flatMap(_.toInt); r <- r.get.flatMap(_.toInt)) yield (q, r))).run == Stream((0, 1)))
    assert((Nat.divmod(Var(Nat(2)), Var(Nat(2)), q, r) &&& (for (q <- q.get.flatMap(_.toInt); r <- r.get.flatMap(_.toInt)) yield (q, r))).run == Stream((1, 0)))
    assert((Nat.divmod(Var(Nat(3)), Var(Nat(2)), q, r) &&& (for (q <- q.get.flatMap(_.toInt); r <- r.get.flatMap(_.toInt)) yield (q, r))).run == Stream((1, 1)))
    assert((Nat.divmod(Var(Nat(5)), Var(Nat(2)), q, r) &&& (for (q <- q.get.flatMap(_.toInt); r <- r.get.flatMap(_.toInt)) yield (q, r))).run == Stream((2, 1)))
    assert((Nat.divmod(Var(Nat(5)), Var(Nat(3)), q, r) &&& (for (q <- q.get.flatMap(_.toInt); r <- r.get.flatMap(_.toInt)) yield (q, r))).run == Stream((1, 2)))
    assert((Nat.divmod(q, Var(Nat(3)), Var(Nat(2)), r) &&& q.get.flatMap(_.toInt)).run.take(1) == Stream(6))
  }

  test("fizzbuzz") {
    def fizzbuzz(n: Var[Nat], s: Var[String]): Logic[Env, Unit] =
      Nat.lt(Var(Nat(0)), n) &&& (s === Var("FizzBuzz") &&& Nat.divmod(n, Var(Nat(15)), Var[Nat], Var(Nat(0)))) ||| (s === Var("Fizz") &&& Nat.divmod(n, Var(Nat(3)), Var[Nat], Var(Nat(0)))) &&& !Nat.divmod(n, Var(Nat(15)), Var[Nat], Var(Nat(0))) ||| (s === Var("Buzz") &&& Nat.divmod(n, Var(Nat(5)), Var[Nat], Var(Nat(0))) &&& !Nat.divmod(n, Var(Nat(15)), Var[Nat], Var(Nat(0))))
    val s = Var[String]
    val n = Var[Nat]
    assert((fizzbuzz(Var(Nat(6)), s) &&& s.get).run == Stream("Fizz"))
    assert((fizzbuzz(Var(Nat(25)), s) &&& s.get).run == Stream("Buzz"))
    assert((fizzbuzz(n, Var("Fizz")) &&& n.get.flatMap(_.toInt)).run.take(5) == Stream(3, 6, 9, 12, 18))
    assert((fizzbuzz(n, Var("Buzz")) &&& n.get.flatMap(_.toInt)).run.take(5) == Stream(5, 10, 20, 25, 35))
    assert((fizzbuzz(n, Var("FizzBuzz")) &&& n.get.flatMap(_.toInt)).run.take(5) == Stream(15, 30, 45, 60, 75))
  }

}
