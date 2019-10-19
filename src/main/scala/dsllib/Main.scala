package dsllib

import cats.implicits._
import cats.{Applicative, Id}
import polymorphic.Exists

import scala.language._

sealed trait Type[A]
case object DoubleType extends Type[Double]
case class FunctionType[T, R](argType: Type[T], retType: Type[R]) extends Type[T => R]

object Type {
  implicit val doubleType: Type[Double] = DoubleType
  implicit def functionType[T, R](implicit t: Type[T], r: Type[R]): Type[T => R] = FunctionType(t, r)
}

sealed trait Expr
case class Var(name: String) extends Expr
case class DoubleLit(value: Double) extends Expr
case class Apply(fun: Expr, arg: Expr) extends Expr

case class TypedValue[F[_], A](value: F[A], tpe: Type[A])

sealed trait Eq[A, B]
object Eq {
  def refl[A]: Eq[A, A] = Refl()
}
case class Refl[A]() extends Eq[A, A]

object Main {

  def typecheck[A, B](typea: Type[A], typeb: Type[B]): Option[Eq[A, B]] = {
    (typea, typeb) match {
      case (DoubleType, DoubleType) =>
        Some(Eq.refl[Double])
      case (f1: FunctionType[arga, reta], f2: FunctionType[argb, retb]) =>
        for {
          argaIsArgb <- typecheck(f1.argType, f2.argType)
          retaIsRetb <- typecheck(f1.retType, f2.retType)
        } yield {
          argaIsArgb match {
            case _: Refl[x] =>
              retaIsRetb match {
                case _: Refl[y] =>
                  Eq.refl[x => y]
              }
          }
        }
      case (_, _) => None
    }
  }


  def compileFuncall[F[_], FunType, ArgType](fun: TypedValue[F, FunType], arg: TypedValue[F, ArgType])(implicit F: Applicative[F]): Option[ETypedValue[F]] = {
    fun.tpe match {
      case DoubleType => None
      case funtype: FunctionType[funArgType, funRetType] =>
        typecheck(funtype.argType, arg.tpe).map { case _: Refl[x] =>
          ETypedValue[F](fun.value <*> arg.value)(funtype.retType)
        }
    }
  }


  def compile[F[_]](expr: Expr, vars: Map[String, ETypedValue[F]])(implicit F: Applicative[F]): Option[ETypedValue[F]] = expr match {
    case Var(name) =>
      vars.get(name)
    case DoubleLit(value) =>
      Some(Exists(TypedValue(F.pure(value), DoubleType)))
    case Apply(fun, arg) =>
      for {
        compiledFun <- compile[F](fun, vars)
        compiledArg <- compile[F](arg, vars)
        compiledApply <- compileFuncall(Exists.unwrap(compiledFun), Exists.unwrap(compiledArg))
      } yield compiledApply
  }

  class castPartiallyApplied[A] {
    def apply[F[_]](typedValue: ETypedValue[F])(implicit targetType: Type[A]): Option[F[A]] = {
      val TypedValue(value, valueType) = Exists.unwrap(typedValue)
      def foo[A, B](eq: Eq[A, B], value: F[A]): F[B] = eq match {
        case _: Refl[A] => value
      }
      typecheck[typedValue.T, A](valueType, targetType).map(foo[typedValue.T, A](_, value))
    }
  }

  def cast[A]: castPartiallyApplied[A] = new castPartiallyApplied[A]

  def main(args: Array[String]): Unit = {
    val expr = Apply(Apply(Var("*"), Apply(Apply(Var("+"), DoubleLit(3)), DoubleLit(5))), DoubleLit(7))
    val vars = Map(
      "+" -> ETypedValue[Id]((a: Double) => (b: Double) => a + b),
      "*" -> ETypedValue[Id]((a: Double) => (b: Double) => a * b)
    )

    (for {
      compiledExpr <- compile(expr, vars)
      typed <- cast[Double](compiledExpr)
    } yield {
      println(typed)
    }).getOrElse {
      println("type checking failed")
    }
  }
}
