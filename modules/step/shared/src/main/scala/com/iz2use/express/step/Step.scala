package com.iz2use.express.step

sealed abstract class Step extends Product with Serializable {
    import Step._

    def foldWith[X](folder: Step.Folder[X]): X

    def ->:(field: Step): Step =
      withObject(o => field +: o)

    def withObject(k: StepObject => StepObject): Step = this match {
      case o @ StepObject(_, _) => k(o)
      case _                    => this
    }

    override def toString: String = Printer().pretty(this)
  }

  final case object StepNull extends Step {
    import Step._
    final def foldWith[X](folder: Folder[X]): X = folder.onNull
  }
  final case class StepReference(value: Long) extends Step {
    import Step._
    final def foldWith[X](folder: Folder[X]): X = folder.onReference(value)
  }
  final case class StepBoolean(value: Boolean) extends Step {
    import Step._
    final def foldWith[X](folder: Folder[X]): X = folder.onBoolean(value)
  }
  final case object StepUnknown extends Step {
    import Step._
    final def foldWith[X](folder: Folder[X]): X = folder.onUnknown
  }
  sealed abstract class StepNumber extends Step {
    private[step] def appendToStringBuilder(builder: StringBuilder): Unit
  }
  final case class StepFloat(value: Float) extends StepNumber {
    import Step._
    final def foldWith[X](folder: Folder[X]): X = folder.onNumber(this)
    private[step] def appendToStringBuilder(builder: StringBuilder): Unit = builder.append(value.toString())
    override def toString() = value.toString()
  }
  final case class StepDouble(value: Double) extends StepNumber {
    import Step._
    final def foldWith[X](folder: Folder[X]): X = folder.onNumber(this)
    private[step] def appendToStringBuilder(builder: StringBuilder): Unit = builder.append(value.toString())
    override def toString() = value.toString()
  }
  final case class StepLong(value: Long) extends StepNumber {
    import Step._
    final def foldWith[X](folder: Folder[X]): X = folder.onNumber(this)
    private[step] def appendToStringBuilder(builder: StringBuilder): Unit = builder.append(value.toString())
    override def toString() = value.toString()
  }
  final case class StepInt(value: Int) extends StepNumber {
    import Step._
    final def foldWith[X](folder: Folder[X]): X = folder.onNumber(this)
    private[step] def appendToStringBuilder(builder: StringBuilder): Unit = builder.append(value.toString())
    override def toString() = value.toString()
  }
  final case class StepString(value: String) extends Step {
    import Step._
    final def foldWith[X](folder: Folder[X]): X = folder.onString(value)
  }
  final case class StepObject(tpe: String, fields: Vector[Step]) extends Step {
    import Step._
    final def foldWith[X](folder: Folder[X]): X = folder.onObject(this)

    private[step] def appendToFolder(folder: Printer.PrintingFolder): Unit = {
      folder.writer.append(tpe.toUpperCase())
      val iterator = fields.iterator
      folder.writer.append('(')
      if (iterator.hasNext) iterator.next().foldWith(folder)
      while (iterator.hasNext) {
        folder.writer.append(',')
        iterator.next().foldWith(folder)
      }
      folder.writer.append(')')
    }

    def +:(field: Step): StepObject =
      copy(fields = field +: fields)

    def :+(field: Step): StepObject =
      copy(fields = fields :+ field)
  }
  final case class StepArray(value: Vector[Step]) extends Step {
    import Step._
    final def foldWith[X](folder: Folder[X]): X = folder.onArray(value)
  }

  object Step {
    final val Null: Step = StepNull
    final val True: Step = StepBoolean(true)
    final val False: Step = StepBoolean(false)
    final val Unknown: Step = StepUnknown

    final def fromBoolean(a: Boolean) = if (a) True else False
    final def fromString(a: String) = StepString(a)
    final def fromInt(a: Int) = StepInt(a)
    final def fromLong(a: Long) = StepLong(a)
    final def fromFloat(a: Float) = StepFloat(a)
    final def fromDouble(a: Double) = StepDouble(a)
    final def arr(values: Step*): Step = fromValues(values)
    final def fromValues(values: Iterable[Step]): Step = StepArray(values.toVector)
    final def fromStepObject(value: StepObject): Step = value

    trait Folder[X] extends Serializable {
      def onNull: X
      def onBoolean(value: Boolean): X
      def onNumber(value: StepNumber): X
      def onString(value: String): X
      def onReference(value: Long): X
      def onArray(value: Vector[Step]): X
      def onObject(value: StepObject): X
      def onUnknown: X
      // onLiteral => Boolean, Logical, Enum ?
    }
  }

  // Make difference between Express, Step, Ifc
  //
  /**
   * In Express, we've these types :
   * - Literals : Logical, Boolean, Binary, String, Number, Integer, Real
   * - Enumerated as Literals
   * - Select Data Type
   * - Defined Data Type
   * - Entity Data Type
   * - Aggregations : Set, Bag, List, Array
   * See WR1 as Refined conditions ?
   */

  /**
   * TYPE A = ENUMERATION OF (
   * X,
   * Y,
   * Z);
   * END_TYPE;
   */

  /**
   * ENTITY A SUBTYPE OF Z;
   *   arg : SET [1:?] OF B;
   *   arg : INTEGER;
   *
   * WHERE
   *   WR01 : Validation(arg);
   * END_ENTITY;
   */

  /**
   * ENTITY A ABSTRACT SUPERTYPE OF (ONEOF(X,Y));
   * END_ENTITY;
   */

  /**
   * FUNCTION A (arg: B) : R;
   * 'code'
   * END_FUNCTION;
   */

  /**
   * We can use directly STEP schema without specialization of types,
   * we juste open STEP schema and use it
   * Or generate all scala classes from a STEP schema
   */
