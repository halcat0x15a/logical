package logical

import org.scalatest.FunSuite

class Poker extends FunSuite {

  case class Card(suit: Var[Suit], n: Var[Nat]) {

    def getString: Logic[Env, String] =
      for {
        s <- suit.get
        n <- n.get
        n <- n.toInt
      } yield s"$s$n"

  }

  object Card {

    implicit def unify: Unify[Card] =
      new Unify[Card] {
        def unify(x: Card, y: Card): Logic[Env, Unit] =
          (x, y) match {
            case (Card(xs, n), Card(ys, m)) => xs === ys &&& n === m
          }
      }

  }

  sealed trait Suit
  case object Heart extends Suit
  case object Diamond extends Suit
  case object Club extends Suit
  case object Spade extends Suit

  object Suit {

    implicit val unify: Unify[Suit] = Unify.unify

  }

  sealed trait Hand
  case object RoyalFlush extends Hand
  case object StraightFlush extends Hand
  case object FourOfAKind extends Hand
  case object FullHouse extends Hand
  case object Flush extends Hand
  case object Straight extends Hand
  case object ThreeOfAKind extends Hand
  case object TwoPairs extends Hand
  case object OnePair extends Hand
  case object NoPair extends Hand

  object Hand {

    implicit val unify: Unify[Hand] = Unify.unify

  }

  def hand(cards: Var[Cons[Card]], hand: Var[Hand]): Logic[Env, Unit] = {
    val x = Var[Suit]
    val n = Var[Nat]
    val m = Var[Nat]
    val patterns = Cons.combinations(Var(Nat(5)), Var(Cons.fromList(List(Heart, Diamond, Club, Spade).flatMap(s => (1 to 13).map(n => Card(Var(s), Var(Nat(n))))))), cards)
    val isOnePair = Cons.combinations(Var(Nat(2)), cards, Var(Cons.fromList(List.fill(2)(Card(Var[Suit], n)))))
    val isTwoPairs = Cons.combinations(Var(Nat(2)), cards, Var(Cons.fromList(List.fill(2)(Card(Var[Suit], n))))) &&& Cons.combinations(Var(Nat(2)), cards, Var(Cons.fromList(List.fill(2)(Card(Var[Suit], m))))) &&& Nat.lteq(Var(Nat(n)), m)
    val isThreeOfAKind = Cons.combinations(Var(Nat(3)), cards, Var(Cons.fromList(List.fill(3)(Card(Var[Suit], n)))))
    val isStraight = cards === Var(Cons.fromList(List.iterate(Card(Var[Suit], n), 5)(card => Card(Var[Suit], Var(Nat(card.n))))))
    val isFlush = cards === Var(Cons.fromList(List.fill(5)(Card(x, Var[Nat]))))
    val isFullHouse = Cons.combinations(Var(Nat(3)), cards, Var(Cons.fromList(List.fill(3)(Card(Var[Suit], n))))) &&& Cons.combinations(Var(Nat(2)), cards, Var(Cons.fromList(List.fill(2)(Card(Var[Suit], m))))) &&& Nat.lteq(Var(Nat(n)), m)
    val isFourOfAKind = Cons.combinations(Var(Nat(4)), cards, Var(Cons.fromList(List.fill(4)(Card(Var[Suit], n)))))
    val isStraightFlush = cards === Var(Cons.fromList(List.iterate(Card(x, n), 5)(card => Card(x, Var(Nat(card.n))))))
    patterns &&& (hand === Var(OnePair) &&& isOnePair ||| hand === Var(TwoPairs) &&& isTwoPairs ||| hand === Var(ThreeOfAKind) &&& isThreeOfAKind ||| hand === Var(Straight) &&& isStraight ||| hand === Var(Flush) &&& isFlush ||| hand === Var(FullHouse) &&& isFullHouse ||| hand === Var(FourOfAKind) &&& isFourOfAKind ||| hand === Var(StraightFlush) &&& isStraightFlush)
  }

  test("poker") {
    val cards = Var[Cons[Card]]
    assert((hand(cards, Var(StraightFlush)) &&& cards.get.flatMap(_.toList).flatMap(cs => Logic.sequence(cs.map(_.getString)))).run.take(3) === Stream())
  }

}
