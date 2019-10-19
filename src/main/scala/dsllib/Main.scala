package dsllib

import cats.evidence.Is
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

object Main {

  def typecheck[A, B](typea: Type[A], typeb: Type[B]): Option[Is[A, B]] = {
    (typea, typeb) match {
      case (DoubleType, DoubleType) =>
        Some(Is.refl)
      case (f1: FunctionType[arga, reta], f2: FunctionType[argb, retb]) =>
        for {
          argaIsArgb <- typecheck(f1.argType, f2.argType)
          retaIsRetb <- typecheck(f1.retType, f2.retType)
        } yield {
          val x1: Is[arga => reta, arga => reta] = Is.refl[arga => reta]
          val x2: Is[arga => reta, argb => reta] = argaIsArgb.substitute[Lambda[X => Is[arga => reta, X => reta]]](x1)
          val x3: Is[arga => reta, argb => retb] = retaIsRetb.substitute[Lambda[X => Is[arga => reta, argb => X]]](x2)
          x3
        }
      case (_, _) => None
    }
  }

  def compileFuncall[F[_], FunType, ArgType](fun: TypedValue[F, FunType], arg: TypedValue[F, ArgType])(implicit F: Applicative[F]): Option[ETypedValue[F]] = {
    fun.tpe match {
      case DoubleType => None
      case funtype: FunctionType[funArgType, funRetType] =>
        typecheck(funtype.argType, arg.tpe).map { is =>
          val funWithAdjustedType = is.substitute[Lambda[X => F[X => funRetType]]](fun.value)
          Exists(TypedValue(funWithAdjustedType <*> arg.value, funtype.retType))
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

  class retrievePartiallyApplied[A] {
    def apply[F[_]](typedValue: ETypedValue[F])(implicit atpe: Type[A]): Option[F[A]] = {
      val TypedValue(value, etpe) = Exists.unwrap(typedValue)
      typecheck(etpe, atpe).map(_.substitute(value))
    }
  }

  def retrieve[A]: retrievePartiallyApplied[A] = new retrievePartiallyApplied[A]

  def main(args: Array[String]): Unit = {
    val expr = Apply(Apply(Var("*"), Apply(Apply(Var("+"), DoubleLit(3)), DoubleLit(5))), DoubleLit(7))
    val vars = Map(
      "+" -> ETypedValue[Id]((a: Double) => (b: Double) => a + b),
      "*" -> ETypedValue[Id]((a: Double) => (b: Double) => a * b)
    )

    (for {
      compiledExpr <- compile(expr, vars)
      typed <- retrieve[Double](compiledExpr)
    } yield {
      println(typed)
    }).getOrElse {
      println("type checking failed")
    }
  }
}
