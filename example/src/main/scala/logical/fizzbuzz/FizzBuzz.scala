package logical

package fizzbuzz

object FizzBuzz extends App {

  val num = Var[Nat]
  val str = Var[String]

  val fizz = Cut(str === "Fizz" &&& Nat.divmod(num, Nat(3), Succ(Var[Nat]), Zero))
  val buzz = Cut(str === "Buzz" &&& Nat.divmod(num, Nat(5), Succ(Var[Nat]), Zero))
  val fizzbuzz = Cut(str === "FizzBuzz" &&& Nat.divmod(num, Nat(15), Succ(Var[Nat]), Zero))

  val app = Nat.nat(num) &&& ((fizzbuzz ||| fizz ||| buzz) &&& str.get ||| num.get.flatMap(_.toInt).map(_.toString))

  app.run.take(100).foreach(println)

}
