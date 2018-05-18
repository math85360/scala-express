package com.iz2use.express.p21

import cats.Contravariant
import shapeless.{ HList, Lazy, HNil, :: }
import shapeless.ops.function.FnFromProduct
import shapeless.ops.record.RemoveAll
import shapeless.Generic

trait ObjectEncoder[A] extends RootEncoder[A] { self =>
  final def apply(a: A)(implicit strictness: EncoderStrictness): Step = Step.fromStepObject(encodeObject(a))

  def encodeObject(a: A)(implicit strictness: EncoderStrictness): StepObject

  final def contramapObject[B](f: B => A): ObjectEncoder[B] = new ObjectEncoder[B] {
    final def encodeObject(a: B)(implicit strictness: EncoderStrictness) = self.encodeObject(f(a))
  }

  final def mapStepObject(f: StepObject => StepObject): ObjectEncoder[A] = new ObjectEncoder[A] {
    final def encodeObject(a: A)(implicit strictness: EncoderStrictness): StepObject = f(self.encodeObject(a))
  }
}

final object ObjectEncoder {
  final def apply[A](implicit instance: ObjectEncoder[A]): ObjectEncoder[A] = instance

  final def instance[A](f: A => StepObject): ObjectEncoder[A] = new ObjectEncoder[A] {
    final def encodeObject(a: A)(implicit strictness: EncoderStrictness): StepObject = f(a)
  }

  //encoder.contramapObject(f).mapStepObject { case StepObject(_, fields) => StepObject(name, fields) }

  implicit final val objectEncoderContravariant: Contravariant[ObjectEncoder] = new Contravariant[ObjectEncoder] {
    final def contramap[A, B](e: ObjectEncoder[A])(f: B => A): ObjectEncoder[B] = e.contramapObject(f)
  }

}