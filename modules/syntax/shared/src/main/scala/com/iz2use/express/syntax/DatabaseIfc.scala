package com.iz2use.express.syntax

case class DatabaseIfc() {
  private var refToItem = scala.collection.immutable.TreeMap[Long, Any]()
  private var itemToRef = Map[Any, Long]()
  private var nextRef: Long = 1

  def counter = nextRef

  def insert[A](a: A): Long =
    itemToRef.get(a) match {
      case Some(v) => v
      case _ =>
        val v = nextRef
        nextRef += 1
        val r = v
        refToItem += r -> a
        itemToRef += a -> r
        r
    }

  override def toString() = {
    s"DatabaseIfc($counter, " +
      refToItem.iterator.map(v => s"#${v._1} = ${v._2}").mkString("\n  ", "\n  ", "\n") + ")"
  }

}
