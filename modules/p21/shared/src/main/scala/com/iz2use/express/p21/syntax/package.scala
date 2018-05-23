package com.iz2use.express.p21

package object syntax {
  implicit final class EncoderOps[A](val wrapperEncodeable: A) extends AnyVal {
    final def asStep(implicit encoder: Encoder[A], context: EncodingContext): Step = encoder(wrapperEncodeable)
    //final def asStepObject(implicit encoder: ObjectEncoder[A]): StepObject = encoder.encodeObject(wrapperEncodeable)
  }
}
