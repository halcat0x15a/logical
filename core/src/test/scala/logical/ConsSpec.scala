package logical

import org.scalatest.FunSpec

class ConsSpec extends FunSpec {

  describe("Cons") {

    it("append") {
      val xs = Var[Cons[Int]]
      val ys = Var[Cons[Int]]
      assert((Cons.append(Var(Cons.fromList(List(1, 2, 3))), Var(Cons.fromList(List(4, 5))), xs) &&& Cons.toList(xs)).run == Stream(List(1, 2, 3, 4, 5)))
      assert((Cons.append(Var(Cons.fromList(List(1, 2, 3))), xs, Var(Cons.fromList(List(1, 2, 3, 4, 5)))) &&& Cons.toList(xs)).run == Stream(List(4, 5)))
      assert((Cons.append(xs, ys, Var(Cons.fromList(List(1, 2, 3)))) &&& (for (xs <- Cons.toList(xs); ys <- Cons.toList(ys)) yield (xs, ys))).run == Stream((Nil, List(1, 2, 3)), (List(1), List(2, 3)), (List(1, 2), List(3)), (List(1, 2, 3), Nil)))
    }

    it("select") {
      val xs = Var[Cons[Int]]
      assert((Cons.select(Var(1), Var(Cons.fromList(List(1, 2, 3))), xs) &&& Cons.toList(xs)).run == Stream(List(2, 3)))
      assert((Cons.select(Var(2), Var(Cons.fromList(List(1, 2, 3))), xs) &&& Cons.toList(xs)).run == Stream(List(1, 3)))
      assert((Cons.select(Var(2), Var(Cons.fromList(List(2, 1, 2, 3))), xs) &&& Cons.toList(xs)).run == Stream(List(1, 2, 3), List(2, 1, 3)))
    }

    it("permutations") {
      val xs = Var[Cons[Int]]
      assert((Cons.permutations(Var(Cons.fromList(List(1, 2, 3))), xs) &&& Cons.toList(xs)).run == Stream(List(1, 2, 3), List(1, 3, 2), List(2, 1, 3), List(2, 3, 1), List(3, 1, 2), List(3, 2, 1)))
    }

    it("combinations") {
      val xs = Var[Cons[Int]]
      assert((Cons.combinations(Var(Nat(2)), Var(Cons.fromList(List(1, 2, 3))), xs) &&& Cons.toList(xs)).run == Stream(List(1, 2), List(1, 3), List(2, 3)))
    }

    it("contains") {
      val xs = Var[Cons[Int]]
      assert(Cons.contains(Var(1), Var(Cons.fromList(List(1, 2, 3)))).run == Stream(()))
      assert(Cons.contains(Var(0), Var(Cons.fromList(List(1, 2, 3)))).run == Stream())
    }

  }

}
