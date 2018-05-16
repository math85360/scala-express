package com.iz2use.express.p21

import scala.annotation.switch
import java.lang.StringBuilder

final case class Printer(
  reuseWriters: Boolean = false) {
  private[this] final class StringBuilderFolder(writer: StringBuilder) extends Printer.PrintingFolder(writer) {

    //final def onNumber(value :StepNumber): Unit = value.appendToStringBuilder(writer)
  }

  @transient
  private[this] final val stringWriter: ThreadLocal[StringBuilder] = new ThreadLocal[StringBuilder] {
    override def initialValue: StringBuilder = new StringBuilder()
  }

  def pretty(step: Step): String = {
    val writer = if (reuseWriters && stringWriter.ne(null)) {
      val w = stringWriter.get()
      w.setLength(0)
      w
    } else new StringBuilder()

    val folder = new StringBuilderFolder(writer)
    step.foldWith(folder)

    writer.toString
  }
}

final object Printer {
  private[p21] abstract class PrintingFolder(
    private[p21] val writer: Appendable) extends Step.Folder[Unit] {
    final def onBoolean(value: Boolean): Unit = writer.append(if (value) ".T." else ".F.")
    final def onNull: Unit = writer.append("$")
    final def onLiteral(value: String) : Unit = {
      writer.append('.')
      writer.append(value)
      writer.append('.')
    }
    final def onReference(value: Long): Unit = {
      writer.append('#')
      writer.append(value.toString)
    }
    final def onUnknown: Unit = writer.append(".U.")

    final def onNumber(number: StepNumber): Unit = writer.append(number.toString)

    final def onString(value: String): Unit = {
      writer.append('\'')
      var i = 0
      var offset = 0
      while (i < value.length) {
        val c = value.charAt(i)

        if (c == '"' || c == '\\' || c == '\b' || c == '\f' || c == '\n' || c == '\r' || c == '\t' ||
          Character.isISOControl(c) ||
          (c.toInt > 126)) {
          writer.append(value, offset, i)
          writer.append('\\')
          (c: @switch) match {
            case '"'  => writer.append('"')
            case '\\' => writer.append('\\')
            case '\b' => writer.append('b')
            case '\f' => writer.append('f')
            case '\n' => writer.append('n')
            case '\r' => writer.append('r')
            case '\t' => writer.append('t')
            case control =>
              writer.append(String.format("u%04x", Integer.valueOf(control.toInt)))
          }
          offset = i + 1
        }
        i += 1
      }
      if (offset < i) writer.append(value, offset, i)
      writer.append('\'')
    }

    final def onArray(value: Vector[Step]): Unit = {
      val iterator = value.iterator
      writer.append('(')
      if (iterator.hasNext) iterator.next().foldWith(this)
      while (iterator.hasNext) {
        writer.append(',')
        iterator.next().foldWith(this)
      }
      writer.append(')')
    }

    final def onObject(value: StepObject): Unit = value.appendToFolder(this)
  }
}