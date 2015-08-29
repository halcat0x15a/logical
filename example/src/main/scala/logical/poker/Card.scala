package logical

package poker

case class Card(suit: Var[Suit], number: Var[Nat])

object Card {

  def apply(): Card = Card(Var[Suit], Var[Nat])

  def values: Cons[Card] =
    Cons.fromList(for (s <- Suit.values; n <- 1 to 13) yield Card(Var(s), Var(Nat(n))))

  def toString(card: Card): Logic[Env, String] =
    for {
      s <- card.suit.get
      n <- Nat.toInt(card.number)
    } yield s"$s($n)"

  def hand(cards: Var[Cons[Card]]): Logic[Env, Unit] =
    Cons.combinations(Var(Nat(5)), Var(values), cards)

  def onePair(cards: Var[Cons[Card]]): Logic[Env, Unit] = {
    val n = Var[Nat]
    Cons.combinations(Var(Nat(2)), cards, Var(Cons.fromList(List.fill(2)(Card(Var[Suit], n)))))
  }

  def twoPairs(cards: Var[Cons[Card]]): Logic[Env, Unit] = {
    val n = Var[Nat]
    val m = Var[Nat]
    Cons.combinations(Var(Nat(2)), cards, Var(Cons.fromList(List.fill(2)(Card(Var[Suit], n))))) &&& Cons.combinations(Var(Nat(2)), cards, Var(Cons.fromList(List.fill(2)(Card(Var[Suit], m))))) &&& Nat.lteq(Var(Nat(n)), m)
  }

  def threeOfAKind(cards: Var[Cons[Card]]): Logic[Env, Unit] = {
    val n = Var[Nat]
    Cons.combinations(Var(Nat(3)), cards, Var(Cons.fromList(List.fill(3)(Card(Var[Suit], n)))))
  }

  def straight(cards: Var[Cons[Card]]): Logic[Env, Unit] = {
    val n = Var[Nat]
    cards === Var(Cons.fromList(List.iterate(Card(Var[Suit], n), 5)(card => Card(Var[Suit], Var(Nat(card.number))))))
  }

  def flush(cards: Var[Cons[Card]]): Logic[Env, Unit] = {
    val s = Var[Suit]
    cards === Var(Cons.fromList(List.fill(5)(Card(s, Var[Nat]))))
  }

  def fullHouse(cards: Var[Cons[Card]]): Logic[Env, Unit] = {
    val n = Var[Nat]
    val m = Var[Nat]
    Cons.combinations(Var(Nat(3)), cards, Var(Cons.fromList(List.fill(3)(Card(Var[Suit], n))))) &&& Cons.combinations(Var(Nat(2)), cards, Var(Cons.fromList(List.fill(2)(Card(Var[Suit], m))))) &&& Nat.lteq(Var(Nat(n)), m)
  }

  def fourOfAKind(cards: Var[Cons[Card]]): Logic[Env, Unit] = {
    val n = Var[Nat]
    Cons.combinations(Var(Nat(4)), cards, Var(Cons.fromList(List.fill(4)(Card(Var[Suit], n)))))
  }

  def straightFlush(cards: Var[Cons[Card]]): Logic[Env, Unit] =
    cards === Var(Cons.fromList(List.iterate(Card(Var[Suit], Var[Nat]), 5)(card => Card(card.suit, Var(Nat(card.number))))))

  implicit def unify: Unify[Card] =
    new Unify[Card] {
      def unify(x: Card, y: Card): Logic[Env, Unit] = x.suit === y.suit &&& x.number === y.number
    }

}
