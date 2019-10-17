import cats.evidence.Is
import polymorphic.Exists

sealed trait Type[A]
case object DoubleType extends Type[Double]
case class FunctionType[T, R](argType: Type[T], retType: Type[R]) extends Type[T => R]

sealed trait Expr
case class Var(name: String) extends Expr
case class DoubleLit(value: Double) extends Expr
case class Apply(fun: Expr, arg: Expr) extends Expr

case class TypedValue[Ctx, A](value: Ctx => A, tpe: Type[A])

object Main {
  type ETypedValue[Ctx] = Exists[TypedValue[Ctx, *]]

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

  def compileFuncall[Ctx, FunType, ArgType](fun: TypedValue[Ctx, FunType], arg: TypedValue[Ctx, ArgType]): Option[ETypedValue[Ctx]] = {
    fun.tpe match {
      case DoubleType => None
      case funtype: FunctionType[funArgType, funRetType] =>
        typecheck(funtype.argType, arg.tpe).map { is =>
          val funWithAdjustedType = is.substitute[Lambda[X => Ctx => X => funRetType]](fun.value)
          Exists(TypedValue((ctx: Ctx) => funWithAdjustedType(ctx)(arg.value(ctx)), funtype.retType))
        }
    }
  }

  def compile[Ctx](expr: Expr, vars: Map[String, ETypedValue[Ctx]]): Option[ETypedValue[Ctx]] = expr match {
    case Var(name) =>
      vars.get(name)
    case DoubleLit(value) =>
      Some(Exists(TypedValue(_ => value, DoubleType)))
    case Apply(fun, arg) =>
      for {
        compiledFun <- compile[Ctx](fun, vars)
        compiledArg <- compile[Ctx](arg, vars)
        compiledApply <- compileFuncall(Exists.unwrap(compiledFun), Exists.unwrap(compiledArg))
      } yield compiledApply
  }

  def typecheck[Ctx, A](typedValue: ETypedValue[Ctx], atpe: Type[A]): Option[Ctx => A] = {
    val TypedValue(value, etpe) = Exists.unwrap(typedValue)
    typecheck(etpe, atpe).map(is => is.substitute(value))
  }

  def main(args: Array[String]): Unit = {
    val expr = Apply(Apply(Var("*"), Apply(Apply(Var("+"), DoubleLit(3)), DoubleLit(5))), DoubleLit(7))
    val vars = Map(
      "+" -> Exists(TypedValue((_: Unit) => (a: Double) => (b: Double) => a + b, FunctionType(DoubleType, FunctionType(DoubleType, DoubleType)))),
      "*" -> Exists(TypedValue((_: Unit) => (a: Double) => (b: Double) => a * b, FunctionType(DoubleType, FunctionType(DoubleType, DoubleType))))
    )

    (for {
      compiledExpr <- compile[Unit](expr, vars)
      typed <- typecheck(compiledExpr, DoubleType)
    } yield {
      println(typed(()))
    }).getOrElse {
      println("type checking failed")
    }
  }
}
