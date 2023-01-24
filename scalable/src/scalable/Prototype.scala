package scalable

object Prototype {

  sealed abstract class Cap[R <: Cap.Abilities, +A] private[Cap] () extends Cap.MappingSyntax[R, A]

  object Cap {

    type Abilities = typeset.TypeSet
    type Pure      = typeset.TNil
    val Pure: Pure = typeset.TNil
    type :&:[A <: Abilities, B <: Abilities] = Nothing // typeset.:+:[A, B]

    def apply[R <: Abilities] = new RequiredPartialApply[R]
    private[Cap] final class RequiredPartialApply[R <: Abilities] {
      def apply(): Cap[R :&: Pure, R] = apply(pure(identity[R]))

      def apply[S <: Abilities, A](f: R => Cap[S, A]): Cap[R :&: S, A] = Required(f)
    }
    private[Cap] final case class Required[R0 <: Abilities, R1 <: Abilities, +A] private[Cap] (f: R0 => Cap[R1, A])
        extends Cap[R0 :&: R1, A]

    private[Cap] trait MappingSyntax[R <: Abilities, +A] { cap: Cap[R, A] =>

      def flatMap[S <: Abilities, B](f: A => Cap[S, B]): Cap[R :&: S, B] = FlatMapped(cap, f)

      def map[B](f: A => B): Cap[R :&: Pure, B] = FlatMapped(cap, pure(f))

      def contraMap[S <: Abilities](f: S => R): Cap[S :&: Pure, A] = contraFlatMap(pure(f))

      def contraFlatMap[S <: Abilities, X <: Abilities](f: S => Cap[X, R]): Cap[S :&: X, A] = into(Cap(f), cap)
    }

    private[Cap] final case class FlatMapped[R0 <: Abilities, A, R1 <: Abilities, +B](
        cap: Cap[R0, A],
        f: A => Cap[R1, B]
    ) extends Cap[R0 :&: R1, B]

    private[Cap] final case class Thunk[+A](value: () => A) extends Cap[Pure, A]

    def pure[A, B](f: A => B): A => Cap[Pure, B] = (a: A) => Thunk(() => f(a))

    def const[A](a: A): Cap[Pure, A] = Thunk(() => a)

    val unit: Cap[Pure, Unit] = const(())

    private[Cap] implicit class PureSyntax[+A](private val cap: Cap[Pure, A]) extends AnyVal {
      def value(): A = Cap.value(cap)
    }

    private[Cap] implicit class ImpureSyntax[R0 <: Abilities, R1 <: Abilities, +A](private val cap: Cap[R0 :&: R1, A])
        extends AnyVal {}

    def value[A](cap: Cap[Pure, A]): A =
      cap match {
        case Thunk(thunk) => thunk()
        case _            => ???
      }

    def into[R <: Abilities] = new IntoPartialApply[R]

    private[Cap] class IntoPartialApply[Rem <: Abilities] {
      def apply[R <: Abilities, X, Y](capX: Cap[R, X], capY: Cap[X :&: Rem, Y]): Cap[R :&: Rem, Y] = {
        //        capX.flatMap { x: X =>
        //          capY match {
        //            case Thunk(thunk)       => Thunk(thunk)
        //            case Required(f)        => ???
        //            case FlatMapped(cap: Cap[X with Rem, Any], f: Nothing => Cap[X with Rem, Y]) =>
        //              cap.contraMap[Rem](rem => /* x with rem */ ???)
        //              f.apply(x)
        //          }
        //        }
        ???
//        capX.flatMap { x: X =>
//          capY.provide(x)
//        }
      }

    }

  }

}

//  object LimitedHere {
//    import Cap.Pure
//
//    object Ask {
//      trait Print {
//        def foo: Cap[Pure, Int] = Cap.const(11)
//      }
//
//      trait Prompt {
//        def apply(prompt: String): Cap[Pure with Print, String] =
//          for {
//            x <- Cap.const("AA")
//            y <- Cap[Print](_.foo).map(_.toString)
//          } yield x + y
//      }
//
//      def prompt(prompt: String): Cap[Prompt with Print with Pure, String] =
//        Cap[Ask.Prompt](_(prompt))
//    }
//
//    def foo: Cap[Ask.Prompt with Ask.Print with Pure, Unit] = {
//      for {
//        _ <- Ask.prompt("Your name")
//        _ <- Ask.prompt("JO")
//      } yield ()
//    }
//
//    val x: Cap[Ask.Print with Pure, Int] =
//      Cap[Ask.Print](_.foo)
//
//  }
