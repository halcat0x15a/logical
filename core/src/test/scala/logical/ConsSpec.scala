package logical

import org.scalatest.FunSpec

class ConsSpec extends FunSpec {

  describe("Cons") {

    it("append") {
      val xs, ys = Var[Cons[Int]]
      assert((Cons.append(Cons(List(1, 2, 3)), Cons(List(4, 5)), xs) &&& xs.get.flatMap(_.toList)).run == Stream(List(1, 2, 3, 4, 5)))
      assert((Cons.append(Cons(List(1, 2, 3)), xs, Cons(List(1, 2, 3, 4, 5))) &&& xs.get.flatMap(_.toList)).run == Stream(List(4, 5)))
      assert((Cons.append(xs, ys, Cons(List(1, 2, 3))) &&& (for (xs <- xs.get.flatMap(_.toList); ys <- ys.get.flatMap(_.toList)) yield (xs, ys))).run == Stream((Nil, List(1, 2, 3)), (List(1), List(2, 3)), (List(1, 2), List(3)), (List(1, 2, 3), Nil)))
    }

    it("select") {
      val xs = Var[Cons[Int]]
      assert((Cons.select(1, Cons(List(1, 2, 3)), xs) &&& xs.get.flatMap(_.toList)).run == Stream(List(2, 3)))
      assert((Cons.select(2, Cons(List(1, 2, 3)), xs) &&& xs.get.flatMap(_.toList)).run == Stream(List(1, 3)))
      assert((Cons.select(2, Cons(List(2, 1, 2, 3)), xs) &&& xs.get.flatMap(_.toList)).run == Stream(List(1, 2, 3), List(2, 1, 3)))
    }

    it("permutations") {
      val xs = Var[Cons[Int]]
      assert((Cons.permutations(Cons(List(1, 2, 3)), xs) &&& xs.get.flatMap(_.toList)).run == Stream(List(1, 2, 3), List(1, 3, 2), List(2, 1, 3), List(2, 3, 1), List(3, 1, 2), List(3, 2, 1)))
    }

    it("combinations") {
      val xs = Var[Cons[Int]]
      assert((Cons.combinations(Nat(2), Cons(List(1, 2, 3)), xs) &&& xs.get.flatMap(_.toList)).run == Stream(List(1, 2), List(1, 3), List(2, 3)))
    }

    it("contains") {
      val xs = Var[Cons[Int]]
      assert(Cons.contains(1, Cons(List(1, 2, 3))).run == Stream(()))
      assert(Cons.contains(0, Cons(List(1, 2, 3))).run == Stream())
    }

  }

}
