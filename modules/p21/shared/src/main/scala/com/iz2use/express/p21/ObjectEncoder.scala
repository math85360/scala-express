package com.iz2use.express.p21

import cats.Contravariant
import shapeless.{ HList, Lazy, HNil, :: }
import shapeless.ops.function.FnFromProduct
import shapeless.ops.record.RemoveAll
import shapeless.Generic

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

abstract class DerivedObjectEncoder[A] extends ObjectEncoder[A]

final object ObjectEncoder {
  final def apply[A](implicit instance: ObjectEncoder[A]): ObjectEncoder[A] = instance

  final def instance[A](f: A => StepObject): ObjectEncoder[A] = new ObjectEncoder[A] {
    final def encodeObject(a: A): StepObject = f(a)
  }

  final def toHList[A, R <: HList](name: String)(f: A => R)(implicit encode: Lazy[ObjectEncoder[R]]): DerivedObjectEncoder[A] = new DerivedObjectEncoder[A] {
    final def encodeObject(a: A): StepObject = encode.value.encodeObject(f(a))
  }
  //encoder.contramapObject(f).mapStepObject { case StepObject(_, fields) => StepObject(name, fields) }

  implicit final val objectEncoderContravariant: Contravariant[ObjectEncoder] = new Contravariant[ObjectEncoder] {
    final def contramap[A, B](e: ObjectEncoder[A])(f: B => A): ObjectEncoder[B] = e.contramapObject(f)
  }

  implicit val hnilEncoder: ObjectEncoder[HNil] =
    instance(hnil => StepObject("", Vector.empty))

  implicit def hlistObjectEncoder[H, T <: HList](
    implicit
    hEncoder: Lazy[Encoder[H]],
    tEncoder: ObjectEncoder[T]): ObjectEncoder[H :: T] = instance { hlist =>
    val head = hEncoder.value.apply(hlist.head)
    val tail = tEncoder.encodeObject(hlist.tail)
    tail.copy(fields = head +: tail.fields)
  }
}