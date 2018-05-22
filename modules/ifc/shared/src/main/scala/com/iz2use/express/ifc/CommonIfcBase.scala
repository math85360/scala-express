package com.iz2use.express.ifc

import com.iz2use.express.p21._
import com.iz2use.express.syntax._
import com.iz2use.express.ifc.ifc4._
import eu.timepit.refined._
import eu.timepit.refined.api._
import java.util.UUID

trait CommonIfcBase {
  private[this] val base64 = "ABCDEFGIHJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789_$"
  private[this] val base64mask = 0x3F
  implicit class RichIfcGloballyUniqueId(root: IfcGloballyUniqueId.type) {
    def next(implicit db: DatabaseIfc): IfcGloballyUniqueId = {
      def c(l: Long, pos: Int): Char = {
        base64.charAt((base64mask & ((l.>>(pos * 6)).toInt)))
      }
      val uuid = UUID.randomUUID()
      val msb = uuid.getMostSignificantBits
      val lsb = uuid.getLeastSignificantBits
      val charArray = Array[Char](
        c(msb, 10), c(msb, 9), c(msb, 8), c(msb, 7), c(msb, 6), c(msb, 5),
        c(msb, 4), c(msb, 3), c(msb, 2), c(msb, 1), c(msb, 0),
        c(lsb, 10), c(lsb, 9), c(lsb, 8), c(lsb, 7), c(lsb, 6), c(lsb, 5),
        c(lsb, 4), c(lsb, 3), c(lsb, 2), c(lsb, 1), c(lsb, 0))
      IfcGloballyUniqueId(Refined.unsafeApply(String.valueOf(charArray)))
    }
  }

  implicit def toRef[A <: ExpressEntity](a: A)(implicit db: DatabaseIfc): RefTo[A] =
    db.insert(a)

  implicit def toRefined[A, P](a: A)(
      implicit
      v: Validate[A, P]): A Refined P =
    refineV[P].apply(a) match {
      case Right(v) => v
      case Left(e)  => throw new Error(s"Not refinable : $a")
    }

  implicit def toOptionRefined[A, P](a: A)(
      implicit
      v: Validate[A, P]): Option[A Refined P] =
    Some(toRefined(a))

  implicit def toOption[A](a: A): Option[A] = Some(a)

  implicit def toOptionRefTo[A <: ExpressEntity](a: A)(implicit db: DatabaseIfc): Option[RefTo[A]] = Some(toRef(a))
}