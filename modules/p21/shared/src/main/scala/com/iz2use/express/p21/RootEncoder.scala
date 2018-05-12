package com.iz2use.express.p21

trait RootEncoder[A] extends Encoder[A]

final object RootEncoder {
  final def apply[A](implicit instance: RootEncoder[A]): RootEncoder[A] = instance
} 
