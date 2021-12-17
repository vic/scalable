package scalable

object Prototype {

  sealed abstract class Cap[-R, +A] private[Cap] () extends Cap.MappingSyntax[R, A]

  object Cap {
//    type Pure = Any
    sealed trait Pure
    def apply[R] = new RequiredPartialApply[R]

    sealed trait Tagged[A, B]
    type `~`[A, B] = Tagged[A, B]

    private[Cap] trait MappingSyntax[-R, +A] { cap: Cap[R, A] =>
      def flatMap[S, B](f: A => Cap[S, B]): Cap[R with S, B] = FlatMapped(cap, f)
      def map[B](f: A => B): Cap[R with Pure, B]             = FlatMapped(cap, pure(f))
      def contraMap[S](f: S => R): Cap[S with Pure, A]       = into[Pure](Cap[S](pure(f)), cap)
    }

    private[Cap] final class RequiredPartialApply[R] {
      def apply(): Cap[R with Pure, R]                     = apply(pure(identity[R]))
      def apply[S, A](f: R => Cap[S, A]): Cap[R with S, A] = Required(f)
    }

    private[Cap] final case class Required[-R0, -R1, +A] private[Cap] (f: R0 => Cap[R1, A])
        extends Cap[R0 with R1, A]

    private[Cap] final case class FlatMapped[-R0, A, -R1, +B](cap: Cap[R0, A], f: A => Cap[R1, B])
        extends Cap[R0 with R1, B]

    private[Cap] final case class Thunk[+A](value: () => A) extends Cap[Pure, A]

    def pure[A, B](f: A => B): A => Cap[Pure, B] = (a: A) => Thunk(() => f(a))

    def const[A](a: A): Cap[Pure, A] = Thunk(() => a)

    val unit: Cap[Pure, Unit] = const(())

    private[Cap] implicit class PureSyntax[+A](private val cap: Cap[Pure, A]) extends AnyVal {
      def value(): A = Cap.value(cap)
    }

    private[Cap] implicit class ImpureSyntax[R0, R1, +A](private val cap: Cap[R0 with R1, A]) extends AnyVal {}

    def value[A](cap: Cap[Pure, A]): A =
      cap match {
        case Thunk(thunk) => thunk()
        case _            => ???
      }

    def into[R] = new IntoPartialApply[R]
    private[Cap] class IntoPartialApply[Rem] {
      def apply[R, X, Y](capX: Cap[R, X], capY: Cap[X with Rem, Y]): Cap[R with Rem, Y] = {
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
      }

    }

  }

  object LimitedHere {
    import Cap.Pure

    object Ask {
      trait Print {
        def foo: Cap[Pure, Int] = Cap.const(11)
      }

      trait Prompt {
        def apply(prompt: String): Cap[Pure with Print, String] =
          for {
            x <- Cap.const("AA")
            y <- Cap[Print](_.foo).map(_.toString)
          } yield x + y
      }

      def prompt(prompt: String): Cap[Prompt with Print with Pure, String] =
        Cap[Ask.Prompt](_(prompt))
    }

    def foo: Cap[Ask.Prompt with Ask.Print with Pure, Unit] = {
      for {
        _ <- Ask.prompt("Your name")
        _ <- Ask.prompt("JO")
      } yield ()
    }

    val x: Cap[Ask.Print with Pure, Int] =
      Cap[Ask.Print](_.foo)

  }
  // ////////////////////////
  // /////////////////////////
  /*

  I => [R] =>  O


   */

  //
  // given M[_].map(x => Eff[Y]) => M[Eff[_]] => Eff[M[_}]
  //
//
//  trait Cap[-I, -C, +O] {
//    def apply(capSet: C): I => O
//  }
//
//  object Cap {
//    def apply[I, C, O](f: I => C => O): Cap[I, C, O] =
//      ???
//  }

//  trait CanFlatMapAbility[-A[-_, +_], -B[-_, +_], +C[-_, +_]]
//  object CanFlatMapAbility {
//    implicit def canChainSameAbility[A[-_, +_]]: CanFlatMapAbility[A, A, A] =
//      ???
//  }
//
//  trait CanFilterAbility[-A[-_, +_], +C[-_, +_]]
//
//  sealed abstract class Ability[+A[-_, +_], +O]() {
//
////    def withFilter[F >: E](cond: A => Boolean)(implicit
////                                               ev: NoSuchElementException <:< F
////    ): Cap[R, F, A]
//    def map[O2](k: O => O2): Ability[A, O2]
//    def flatMap[B[-_, +_], O2, C[-_, +_]](k: O => Ability[B, O2])(implicit
//        ev: CanFlatMapAbility[A, B, C]
//    ): Ability[C, O2]
//    def withFilter[B[-_, +_]](cond: O => Boolean)(implicit ev: CanFilterAbility[A, B]): Ability[B, O]
//  }
//
//  trait Cap[A[-_, +_]] {
//    def receive[O](pure: A => O): Ability[A, O] = ???
//  }
//
//  trait Ask[-R, +E] {
//    def ask(what: String): String
//  }
//  object Ask extends Cap[Ask] {
//    def ask(what: String): Ability[Ask, String] =
//      receive(_.ask(what))
//  }
//
//  def foo = {
//    val x: Ability[Ask, Int] = for {
//      _ <- Ask.ask("hello")
//      _ <- Ask.ask("hello")
//    } yield 33
//  }

}
