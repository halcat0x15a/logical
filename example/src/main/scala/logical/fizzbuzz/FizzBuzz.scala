package logical

package fizzbuzz

object FizzBuzz extends App {

  val num = Var[Nat]
  val str = Var[String]

  val fizz = str === Var("Fizz") &&& Nat.divmod(num, Var(Nat(3)), Var(Nat(Var[Nat])), Var(Nat()))
  val buzz = str === Var("Buzz") &&& Nat.divmod(num, Var(Nat(5)), Var(Nat(Var[Nat])), Var(Nat()))
  val fizzbuzz = str === Var("FizzBuzz") &&& Nat.divmod(num, Var(Nat(15)), Var(Nat(Var[Nat])), Var(Nat()))

  val app = Nat.nat(num) &&& ((Logic.cut(fizzbuzz) ||| fizz ||| buzz) &&& str.get ||| num.get.flatMap(_.toInt).map(_.toString))

  app.run.take(100).foreach(println)

}
