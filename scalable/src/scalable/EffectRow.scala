package scalable

import scala.annotation.{implicitAmbiguous, implicitNotFound, tailrec}
import scala.language.implicitConversions

// Effect Rows https://youtu.be/N9EBKp5nTzU?t=424
// Scala type-level sets https://www.youtube.com/watch?v=N_it0qwk7_U&t=4374s
object EffectRow {

  // Handler set
  sealed trait TSet

  @implicitNotFound("Cannot use ${E} as head of ${T}")
  sealed trait CanBeHeadedWith[E, T <: TSet]
  object CanBeHeadedWith {
    private def instance[E, T <: TSet] = new CanBeHeadedWith[E, T] {}

    @implicitAmbiguous("Not possible to use TNil as head")
    implicit val cannotHeadNil: CanBeHeadedWith[TNil, TNil] = instance
    implicit val cannotHeadNil0: CanBeHeadedWith[TNil, TNil] = instance

    implicit def canHeadAnyToTNil[E]: CanBeHeadedWith[E, TNil] = instance

    @implicitAmbiguous("Cannot use set ${E} as head of ${S}")
    implicit def cannotHeadWithSet[E <: TSet, S <: TSet]: CanBeHeadedWith[E, S] = instance
    implicit def cannotHeadWithSet0[E <: TSet, S <: TSet]: CanBeHeadedWith[E, S] = instance

    @implicitAmbiguous("Cannot use ${H} as head of ${H} :+: ${T}")
    implicit def cannotSameHead[H, T <: TSet](): CanBeHeadedWith[H, H :+: T] = instance
    implicit def cannotSameHead0[H, T <: TSet](): CanBeHeadedWith[H, H :+: T] = instance

    implicit def canHeadAnySet[E, H, T <: TSet](implicit
        ev: CanBeHeadedWith[E, T]
    ): CanBeHeadedWith[E, H :+: T] = instance

  }

  @implicitNotFound("Cannot prepend ${A} :+: ${I} into ${H} :+: ${T}")
  sealed trait CanBePrependedWith[A, I <: TSet, H, T <: TSet, O, OT <: TSet]

  object CanBePrependedWith {
    private def instance[A, I <: TSet, H, T <: TSet, O, OT <: TSet] =
      new CanBePrependedWith[A, I, H, T, O, OT] {}

    implicit def canBePrependedWithSingle[A, H, T <: TSet](
        implicit ev1: CanBeHeadedWith[A, H :+: T]
    ): CanBePrependedWith[A, TNil, H, T, A, H :+: T] = instance

    implicit def canBePrependedWithSet[A, B, C <: TSet, H, T <: TSet, O, OT <: TSet](
        implicit
        ev1: CanBePrependedWith[B, C, H, T, O, OT],
        ev2: CanBeHeadedWith[A, O :+: OT]
    ): CanBePrependedWith[A, B :+: C, H, T, A, O :+: OT] = instance

  }

  final case class CanDrop[X, H, T <: TSet, O <: TSet](idx: Int)
  object CanDrop {
    private def instance[X, H, T <: TSet, O <: TSet](idx: Int) =
      CanDrop[X, H, T, O](idx)

    implicit def canDropFromHead[H, T <: TSet]: CanDrop[H, H, T, T] =
      instance(0)

    implicit def canDropFromSet[X, H, I, T <: TSet, O <: TSet](
        implicit ev0: CanDrop[X, I, T, O]
    ): CanDrop[X, H, I :+: T, H :+: O] = instance(ev0.idx + 1)

    class DropPartiallyApplied[X, H, T <: TSet](seq: Seq[Any]) {
      def apply[O, OT <: TSet]()(implicit ev: CanDrop[X, H, T, O :+: OT]): (O :+: OT, X) = {
        val set = new :+:[O, OT](seq.slice(0, ev.idx + 1) ++ seq.drop(ev.idx + 1))
        (set, seq.apply(ev.idx).asInstanceOf[X])
      }
    }
  }

  sealed trait CanReorg[A, I <: TSet, H, T <: TSet, O <: TSet]
  object CanReorg {
    private def instance[A, I <: TSet, H, T <: TSet, O <: TSet] =
      new CanReorg[A, I, H, T, O] {}

    implicit def canReorgNil[A]: CanReorg[A, TNil, A, TNil, A :+: TNil] = instance

    implicit def canReorgFoo[A, B, C <: TSet, H, T <: TSet, O <: TSet, D, DA <: TSet](
        implicit
        ev0: CanDrop[A, H, T, D :+: DA],
        ev1: CanReorg[B, C, D, DA, O]
    ): CanReorg[A, B :+: C, H, T, A :+: O] = instance

  }

  final class :+:[H, T <: TSet](protected val seq: Seq[Any]) extends TSet {
    def :+:[E](e: E)(implicit ev: CanBeHeadedWith[E, H :+: T]): E :+: H :+: T =
      new :+:(e +: seq)

    def :+:[A, I <: TSet, O, OT <: TSet](init: A :+: I)(
        implicit ev: CanBePrependedWith[A, I, H, T, O, OT]
    ): O :+: OT = new :+:[O, OT](init.seq ++ seq)

    def get[A](implicit ev: CanDrop[A, H, T, _]): A = {
      seq.apply(ev.idx).asInstanceOf[A]
    }

    def drop[X] = new CanDrop.DropPartiallyApplied[X, H, T](seq)

    implicit def reorg[A, I <: TSet, O <: TSet](
        implicit ev: CanReorg[A, I, H, T, O]
    ): O = ???
  }
  object :+: {}

  sealed trait TNil extends TSet
  object TNil extends TNil {
    def :+:[E](e: E)(implicit ev: CanBeHeadedWith[E, TNil]): E :+: TNil =
      new :+:(Seq(e))
  }

}

object ExampleRow extends App {
  import EffectRow._

  val a = 1 :+: TNil
  val b = None :+: true :+: "ja" :+: TNil
  val c = b :+: a
  val d = a :+: c // should fail // FIX ME

//  val a = true :+: 1 :+: TNil
//
//  val x: Boolean :+: TNil = a.drop[Int]()
//
  val zr      = "JAJA" :+: 1 :+: true :+: TNil
  val (z, zz) = zr.drop[Boolean]()
  println(zz)

//  val u: Int :+: Boolean :+: TNil = a.reorg

//  val x                = 1 :+: TNil
//  val x1: Int :+: TNil = x.reorg

  val m = true :+: "JAJA" :+: 1 :+: TNil

  println(m.get[Int])

//  val m1: Int :+: Boolean :+: TNil = m.reorg

}
