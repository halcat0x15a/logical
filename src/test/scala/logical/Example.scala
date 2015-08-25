package logical

import org.scalatest.FunSuite

class Example extends FunSuite {

  test("Cons.append") {
    val xs = Var[Cons[Int]]
    val ys = Var[Cons[Int]]
    assert((Cons.append(Var(Cons.fromList(List(1, 2, 3))), Var(Cons.fromList(List(4, 5))), xs) &&& xs.get.flatMap(_.toList)).run == Stream(List(1, 2, 3, 4, 5)))
    assert((Cons.append(Var(Cons.fromList(List(1, 2, 3))), xs, Var(Cons.fromList(List(1, 2, 3, 4, 5)))) &&& xs.get.flatMap(_.toList)).run == Stream(List(4, 5)))
    assert((Cons.append(xs, ys, Var(Cons.fromList(List(1, 2, 3)))) &&& (for (xs <- xs.get.flatMap(_.toList); ys <- ys.get.flatMap(_.toList)) yield (xs, ys))).run == Stream((Nil, List(1, 2, 3)), (List(1), List(2, 3)), (List(1, 2), List(3)), (List(1, 2, 3), Nil)))
  }

  test("Cons.select") {
    val xs = Var[Cons[Int]]
    assert((Cons.select(Var(1), Var(Cons.fromList(List(1, 2, 3))), xs) &&& xs.get.flatMap(_.toList)).run == Stream(List(2, 3)))
    assert((Cons.select(Var(2), Var(Cons.fromList(List(1, 2, 3))), xs) &&& xs.get.flatMap(_.toList)).run == Stream(List(1, 3)))
    assert((Cons.select(Var(2), Var(Cons.fromList(List(2, 1, 2, 3))), xs) &&& xs.get.flatMap(_.toList)).run == Stream(List(1, 2, 3), List(2, 1, 3)))
  }

  test("Cons.permutations") {
    val xs = Var[Cons[Int]]
    assert((Cons.permutations(Var(Cons.fromList(List(1, 2, 3))), xs) &&& xs.get.flatMap(_.toList)).run == Stream(List(1, 2, 3), List(1, 3, 2), List(2, 1, 3), List(2, 3, 1), List(3, 1, 2), List(3, 2, 1)))
  }

  test("Cons.combinations") {
    val xs = Var[Cons[Int]]
    assert((Cons.combinations(Var(Nat(2)), Var(Cons.fromList(List(1, 2, 3))), xs) &&& xs.get.flatMap(_.toList)).run == Stream(List(1, 2), List(1, 3), List(2, 3)))
  }

  test("Cons.contains") {
    val xs = Var[Cons[Int]]
    assert(Cons.contains(Var(1), Var(Cons.fromList(List(1, 2, 3)))).run == Stream(()))
    assert(Cons.contains(Var(0), Var(Cons.fromList(List(1, 2, 3)))).run == Stream())
  }

  test("Nat.plus") {
    val x = Var[Nat]
    val y = Var[Nat]
    assert((Nat.plus(Var(Nat(2)), Var(Nat(3)), x) &&& Nat.toInt(x)).run == Stream(5))
    assert((Nat.plus(Var(Nat(2)), x, Var(Nat(5))) &&& Nat.toInt(x)).run == Stream(3))
    assert((Nat.plus(x, Var(Nat(2)), Var(Nat(5))) &&& Nat.toInt(x)).run == Stream(3))
    assert((Nat.plus(x, y, Var(Nat(3))) &&& (for (x <- Nat.toInt(x); y <- Nat.toInt(y)) yield (x, y))).run == Stream((0, 3), (1, 2), (2, 1), (3, 0)))
    assert((Nat.plus(x, Var(Nat(3)), y) &&& (for (x <- Nat.toInt(x); y <- Nat.toInt(y)) yield (x, y))).run.take(3) == Stream((0, 3), (1, 4), (2, 5)))
    assert((Nat.plus(Var(Nat(3)), x, y) &&& (for (x <- Nat.toInt(x); y <- Nat.toInt(y)) yield (x, y))).run.take(3) == Stream((0, 3), (1, 4), (2, 5)))
  }

  test("Nat.lteq") {
    val n = Var[Nat]
    assert(Nat.lteq(Var(Nat(2)), Var(Nat(3))).run == Stream(()))
    assert(Nat.lteq(Var(Nat(3)), Var(Nat(3))).run == Stream(()))
    assert(Nat.lteq(Var(Nat(4)), Var(Nat(3))).run == Stream())
    assert((Nat.lteq(n, Var(Nat(3))) &&& Nat.toInt(n)).run == Stream(0, 1, 2, 3))
    assert((Nat.lteq(Var(Nat(1)), n) &&& Nat.toInt(n)).run.take(3) == Stream(1, 2, 3))
  }

  test("Nat.mod") {
    val q = Var[Nat]
    val r = Var[Nat]
    assert((Nat.divmod(Var(Nat(1)), Var(Nat(2)), q, r) &&& (for (q <- Nat.toInt(q); r <- Nat.toInt(r)) yield (q, r))).run == Stream((0, 1)))
    assert((Nat.divmod(Var(Nat(2)), Var(Nat(2)), q, r) &&& (for (q <- Nat.toInt(q); r <- Nat.toInt(r)) yield (q, r))).run == Stream((1, 0)))
    assert((Nat.divmod(Var(Nat(3)), Var(Nat(2)), q, r) &&& (for (q <- Nat.toInt(q); r <- Nat.toInt(r)) yield (q, r))).run == Stream((1, 1)))
    assert((Nat.divmod(Var(Nat(5)), Var(Nat(2)), q, r) &&& (for (q <- Nat.toInt(q); r <- Nat.toInt(r)) yield (q, r))).run == Stream((2, 1)))
    assert((Nat.divmod(Var(Nat(5)), Var(Nat(3)), q, r) &&& (for (q <- Nat.toInt(q); r <- Nat.toInt(r)) yield (q, r))).run == Stream((1, 2)))
    assert((Nat.divmod(q, Var(Nat(3)), Var(Nat(2)), r) &&& Nat.toInt(q)).run.take(1) == Stream(6))
  }

  test("FizzBuzz") {
    def fizzbuzz(n: Var[Nat], s: Var[String]): Logic[Env, Unit] = {
      val isFizz = Nat.divmod(n, Var(Nat(3)), Var(Nat(Var[Nat])), Var(Nat()))
      val isBuzz = Nat.divmod(n, Var(Nat(5)), Var(Nat(Var[Nat])), Var(Nat()))
      val isFizzBuzz = Nat.divmod(n, Var(Nat(15)), Var(Nat(Var[Nat])), Var(Nat()))
      (s === Var("FizzBuzz") &&& isFizzBuzz) ||| (s === Var("Fizz") &&& isFizz &&& !isFizzBuzz) ||| (s === Var("Buzz") &&& isBuzz &&& !isFizzBuzz)
    }
    val s = Var[String]
    val n = Var[Nat]
    assert((fizzbuzz(Var(Nat(6)), s) &&& s.get).run == Stream("Fizz"))
    //assert((fizzbuzz(Var(Nat(25)), s) &&& s.get).run == Stream("Buzz"))
    //assert((fizzbuzz(Var(Nat(45)), s) &&& s.get).run == Stream("FizzBuzz"))
    //assert((fizzbuzz(n, Var("Fizz")) &&& n.get.flatMap(_.toInt)).run.take(5) == Stream(3, 6, 9, 12, 18))
    //assert((fizzbuzz(n, Var("Buzz")) &&& n.get.flatMap(_.toInt)).run.take(5) == Stream(5, 10, 20, 25, 35))
    //assert((fizzbuzz(n, Var("FizzBuzz")) &&& n.get.flatMap(_.toInt)).run.take(5) == Stream(15, 30, 45, 60, 75))
    //assert((Nat.lteq(Var(Nat(1)), n) &&& (fizzbuzz(n, s) &&& s.get ||| !fizzbuzz(n, s) &&& n.get.flatMap(_.toInt).map(_.toString))).run.take(5) == Stream("1", "2", "Fizz", "4", "Buzz"))
  }

}
