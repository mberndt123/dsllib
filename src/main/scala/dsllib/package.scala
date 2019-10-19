import polymorphic.Exists
import scala.language._

package object dsllib {
  type ETypedValue[F[_]] = Exists[TypedValue[F, *]]
  object ETypedValue {
    class ETypedValuePartiallyApplied[F[_]] {
      def apply[A](fa: F[A])(implicit tpe: Type[A]): ETypedValue[F] = Exists(TypedValue(fa, tpe))
    }

    def apply[F[_]]: ETypedValuePartiallyApplied[F] = new ETypedValuePartiallyApplied[F]
  }

}
