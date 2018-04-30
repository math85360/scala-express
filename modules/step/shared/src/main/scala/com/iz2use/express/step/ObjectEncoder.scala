package com.iz2use.express.step

import cats.Contravariant
import shapeless.{ Default, Strict, Generic, HList, LabelledGeneric, Lazy, HNil, :: }
import shapeless.ops.function.FnFromProduct
import shapeless.ops.record.RemoveAll

trait ObjectEncoder[A] extends RootEncoder[A] { self =>
  final def apply(a: A): Step = Step.fromStepObject(encodeObject(a))

  def encodeObject(a: A): StepObject

  final def contramapObject[B](f: B => A): ObjectEncoder[B] = new ObjectEncoder[B] {
    final def encodeObject(a: B) = self.encodeObject(f(a))
  }

  final def mapStepObject(f: StepObject => StepObject): ObjectEncoder[A] = new ObjectEncoder[A] {
    final def encodeObject(a: A): StepObject = f(self.encodeObject(a))
  }
}

final object ObjectEncoder {
  final def apply[A](implicit instance: ObjectEncoder[A]): ObjectEncoder[A] = instance

  final def instance[A](f: A => StepObject): ObjectEncoder[A] = new ObjectEncoder[A] {
    final def encodeObject(a: A): StepObject = f(a)
  }

  implicit final val objectEncoderContravariant: Contravariant[ObjectEncoder] = new Contravariant[ObjectEncoder] {
    final def contramap[A, B](e: ObjectEncoder[A])(f: B => A): ObjectEncoder[B] = e.contramapObject(f)
  }

  implicit val hnilEncoder: ObjectEncoder[HNil] =
    instance(hnil => StepObject("", Vector.empty))

  implicit def hlistObjectEncoder[H, T <: HList](
    implicit
    hEncoder: Lazy[Encoder[H]],
    tEncoder: ObjectEncoder[T]
  ): ObjectEncoder[H :: T] = instance { hlist =>
    val head = hEncoder.value.apply(hlist.head)
    val tail = tEncoder.encodeObject(hlist.tail)
    tail.copy(fields = head +: tail.fields)
  }
}