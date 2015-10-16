/*import scala.language.{higherKinds, implicitConversions}

package object logical {

  implicit def wrapVar[A](value: A): Var[A] = Var(value)

  implicit def liftVar[F[_], A](value: F[A])(implicit F: kits.Functor[F]): F[Var[A]] = F.map(value)(Var(_))

}
 */
