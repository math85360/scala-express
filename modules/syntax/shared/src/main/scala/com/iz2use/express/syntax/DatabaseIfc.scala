package com.iz2use.express.syntax

case class DatabaseIfc() {
  private var refToItem = scala.collection.immutable.TreeMap[RefTo[_], Any]()
  private var itemToRef = Map[Any, RefTo[_]]()
  private var nextRef: Long = 1

  def counter = nextRef

  def insert[A](a: A): RefTo[A] =
    itemToRef.get(a) match {
      case Some(v: RefTo[A]) => v
      case _ =>
        val v = nextRef
        nextRef += 1
        val r = RefTo[A](v)
        refToItem += r -> a
        itemToRef += a -> r
        r
    }

  def update[A](ref: RefTo[A], newValue: A): Unit = {
    val oldValue = refToItem(ref)
    refToItem = refToItem.updated(ref, newValue)
    itemToRef = (itemToRef - oldValue) + (newValue -> ref)
  }

  def update[A](oldValue: A, newValue: A): Unit = {
    val ref = itemToRef(oldValue)
    refToItem = refToItem.updated(ref, newValue)
    itemToRef = (itemToRef - oldValue) + (newValue -> ref)
  }

  def remove[A](ref: RefTo[A]): Unit = {
    val value = refToItem(ref)
    refToItem = refToItem - ref
    itemToRef = (itemToRef - value)
  }

  def remove[A](value: A): Unit = {
    val ref = itemToRef(value)
    refToItem = refToItem - ref
    itemToRef = (itemToRef - value)
  }

  def apply[A](a: RefTo[A]): Option[A] =
    refToItem.get(a) match {
      case Some(r: A) => Some(r)
      case _          => None
    }

  override def toString() = {
    s"DatabaseIfc($counter, " +
      refToItem.iterator.map(v => s"#${v._1} = ${v._2}").mkString("\n  ", "\n  ", "\n") + ")"
  }

}
