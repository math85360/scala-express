package com.iz2use.express.step

trait RootEncoder[A] extends Encoder[A]

final object RootEncoder {
  final def apply[A](implicit instance: RootEncoder[A]): RootEncoder[A] = instance
} 
