package logical

package poker

case class Card(suit: Var[Suit], number: Var[Nat])

object Card {

  val values: Cons[Card] =
    Cons(List(Heart, Diamond, Club, Spade).flatMap(s => (1 to 13).map(n => Card(s, Nat(n)))))

  def hand(cards: Var[Cons[Card]]): Logic[Unit] =
    Cons.combinations(Nat(5), values, cards)

  def onePair(cards: Var[Cons[Card]]): Logic[Unit] = {
    val n = Var[Nat]
    Cons.combinations(Nat(2), cards, Cons(List.fill(2)(Card(Var[Suit], n))))
  }

  def twoPairs(cards: Var[Cons[Card]]): Logic[Unit] = {
    val n, m = Var[Nat]
    Cons.combinations(Nat(2), cards, Cons(List.fill(2)(Card(Var[Suit], n)))) &&& Cons.combinations(Nat(2), cards, Cons(List.fill(2)(Card(Var[Suit], m)))) &&& Nat.lteq(Succ(n), m)
  }

  def threeOfAKind(cards: Var[Cons[Card]]): Logic[Unit] = {
    val n = Var[Nat]
    Cons.combinations(Nat(3), cards, Cons(List.fill(3)(Card(Var[Suit], n))))
  }

  def straight(cards: Var[Cons[Card]]): Logic[Unit] = {
    val n = Var[Nat]
    Cons.permutations(cards, Cons(List.iterate(Card(Var[Suit], n), 5)(card => Card(Var[Suit], Succ(card.number)))))
  }

  def flush(cards: Var[Cons[Card]]): Logic[Unit] = {
    val s = Var[Suit]
    cards === Cons(List.fill(5)(Card(s, Var[Nat])))
  }

  def fullHouse(cards: Var[Cons[Card]]): Logic[Unit] = {
    val n = Var[Nat]
    val m = Var[Nat]
    Cons.combinations(Nat(3), cards, Cons(List.fill(3)(Card(Var[Suit], n)))) &&& Cons.combinations(Nat(2), cards, Cons(List.fill(2)(Card(Var[Suit], m)))) &&& Nat.lteq(Succ(n), m)
  }

  def fourOfAKind(cards: Var[Cons[Card]]): Logic[Unit] = {
    val n = Var[Nat]
    Cons.combinations(Nat(4), cards, Cons(List.fill(4)(Card(Var[Suit], n))))
  }

  def straightFlush(cards: Var[Cons[Card]]): Logic[Unit] =
    cards === Cons(List.iterate(Card(Var[Suit], Var[Nat]), 5)(card => Card(card.suit, Succ(card.number))))

  implicit def unify: Unify[Card] =
    new Unify[Card] {
      def unify(x: Card, y: Card): Logic[Unit] = x.suit === y.suit &&& x.number === y.number
    }

}
