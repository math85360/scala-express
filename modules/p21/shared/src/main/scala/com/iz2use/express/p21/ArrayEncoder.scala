package com.iz2use.express.p21

import cats.Contravariant

trait ArrayEncoder[A] extends RootEncoder[A] { self =>
  final def apply(a: A)(implicit context: EncodingContext): Step = Step.fromValues(encodeArray(a))

  def encodeArray(a: A)(implicit context: EncodingContext): Vector[Step]

  final def contramapArray[B](f: B => A): ArrayEncoder[B] = new ArrayEncoder[B] {
    final def encodeArray(a: B)(implicit context: EncodingContext) = self.encodeArray(f(a))
  }

  final def mapStepArray(f: Vector[Step] => Vector[Step]): ArrayEncoder[A] = new ArrayEncoder[A] {
    final def encodeArray(a: A)(implicit context: EncodingContext): Vector[Step] = f(self.encodeArray(a))
  }
}

final object ArrayEncoder {
  final def apply[A](implicit instance: ArrayEncoder[A]): ArrayEncoder[A] = instance

  final def instance[A](f: A => Vector[Step]): ArrayEncoder[A] = new ArrayEncoder[A] {
    final def encodeArray(a: A)(implicit context: EncodingContext): Vector[Step] = f(a)
  }

  implicit final val ArrayEncoderContravariant: Contravariant[ArrayEncoder] = new Contravariant[ArrayEncoder] {
    final def contramap[A, B](e: ArrayEncoder[A])(f: B => A): ArrayEncoder[B] = e.contramapArray(f)
  }
}

/*private[step] trait LowPriorityArrayEncoders {
  implicit final def importedArrayEncoder[A](
    implicit
    exported: Exported[ArrayEncoder[A]]
  ): ArrayEncoder[A] = exported.instance

}
*/
