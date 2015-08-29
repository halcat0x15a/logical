package logical

package poker

sealed trait Suit

case object Heart extends Suit
case object Diamond extends Suit
case object Club extends Suit
case object Spade extends Suit

object Suit {

  val values: List[Suit] = List(Heart, Diamond, Club, Spade)

  implicit val unify: Unify[Suit] = Unify.unify

}
