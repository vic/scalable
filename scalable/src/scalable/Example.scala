package scalable

import scala.util.{Success, Try}

object Example {

  trait Ability[R] {
    def receive[E, A](handle: R => Resume[E, A]): Cap[R, E, A] =
      new Cap.Received[R, E, A](this, handle)
    def succeed[A](a: => A): Cap[R, Nothing, A] =
      new Cap.Succeed(this)(() => a)
    def failed[E](e: => E): Cap[R, E, Nothing] =
      new Cap.Failed(this)(() => e)
  }

  object Ability {}

  sealed abstract class Cap[-R, +E, +A] private[Cap] () {
    def withFilter[F >: E](cond: A => Boolean)(implicit
        ev: NoSuchElementException <:< F
    ): Cap[R, F, A]
    def map[B](k: A => B): Cap[R, E, B]
    def flatMap[S, F >: E, B](k: A => Cap[S, F, B]): Cap[R with S, F, B]
  }

  object Cap {

    class ForSyntax[R, E, A](val cap: Ability[R]) extends Cap[R, E, A] {
      override def map[B](k: A => B): Cap[R, E, B] =
        new Mapped(cap, k)

      override def flatMap[S, F >: E, B](
          k: A => Cap[S, F, B]
      ): Cap[R with S, F, B] =
        new FlatMapped[R, A, S, F, B](cap, k)

      def withFilter[F >: E](cond: A => Boolean)(implicit
          ev: NoSuchElementException <:< F
      ): Cap[R, F, A] =
        new Filtered[R, F, A](cap, cond)
    }

    class Succeed[R, A](cap: Ability[R])(val value: () => A)
        extends ForSyntax[R, Nothing, A](cap)

    class Failed[R, E](cap: Ability[R])(val value: () => E)
        extends ForSyntax[R, E, Nothing](cap)

    class Received[R, E, A](cap: Ability[R], val handler: R => Resume[E, A])
        extends ForSyntax[R, E, A](cap)

    class Filtered[R, E, A](
        cap: Ability[R],
        val cond: A => Boolean
    ) extends ForSyntax[R, E, A](cap)

    class Mapped[R, E, A, B](
        cap: Ability[R],
        val k: A => B
    ) extends ForSyntax[R, E, A](cap)

    class FlatMapped[R, A, S, F, B](
        cap: Ability[R],
        val k: A => Cap[S, F, B]
    ) extends ForSyntax[R, F, B](cap)
  }

  sealed trait Resume[+E, +A]
  object Resume {

    trait Transform[+E, +A] { self: Resume[E, A] =>
      def mapError[F](m: E => F): Resume[F, A]
      def mapBoth[F, B](mE: E => F, mA: A => B): Resume[F, B]
    }

    trait ForSyntax[+E, +A] { self: Resume[E, A] =>
      def map[B](m: A => B): Resume[E, B]
      def flatMap[F >: E, B](m: A => Resume[F, B]): Resume[F, B]
      def withFilter[F >: E](cond: A => Boolean)(implicit ev: NoSuchElementException <:< F): Resume[F, A]
    }

    trait Effect[Z[-_, +_, +_], +E, +A] extends Resume[E, A]

    final case class Success[A](value: A) extends Resume[Nothing, A]
    final case class Failure[E](value: E) extends Resume[E, Nothing]

    def cond[E, A](condition: Boolean)(whenTrue: => A)(whenFalse: => E): Resume[E, A] =
      if (condition) Success(whenTrue) else Failure(whenFalse)

    def fromEither[E, A](e: Either[E, A]): Resume[E, A] =
      e match {
        case Left(value)  => Failure(value)
        case Right(value) => Success(value)
      }

    def fromOption[A](o: Option[A]): Resume[NoSuchElementException, A] =
      o match {
        case Some(value) => Success(value)
        case None        => Failure(new NoSuchElementException)
      }

    def fromTry[A](t: Try[A]): Resume[Throwable, A] =
      t match {
        case util.Failure(exception) => Failure(exception)
        case util.Success(value)     => Success(value)
      }
  }

  trait Log {
    def log(msg: String): Resume[Throwable, Unit]
  }

  object Log extends Ability[Log] {
    def log(msg: String): Cap[Log, Throwable, Unit] =
      receive(_.log(msg))
  }

  trait Ask {
    def ask(what: String): Resume[Nothing, String]
  }

  object Ask extends Ability[Ask] {
    def ask(what: String): Cap[Ask, Nothing, String] =
      receive(_.ask(what))
  }

  def hello(who: String): Cap[Ask with Log, Nothing, Int] =
    for {
      name <- Ask.ask("Whats your name?")
//      _ <- Log.log(s"hello ${name}")
    } yield 33

  def runHello: Int = {
//    Ask(hello("world"))
    ???
  }

}
