import scala.language.{higherKinds, implicitConversions}

import kits.Functor

package object logical {

  implicit def wrapVar[A](value: A): Var[A] = Var(value)

  implicit def liftVar[F[_], A](value: F[A])(implicit F: Functor[F]): F[Var[A]] = F.map(value)(Var(_))

}
