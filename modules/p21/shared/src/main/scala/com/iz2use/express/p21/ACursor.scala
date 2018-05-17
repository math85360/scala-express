package com.iz2use.express.p21

abstract class ACursor(private val lastCursor: HCursor, private val lastOp: CursorOp) extends Serializable {
  final def history: List[CursorOp] = {
    var next = this
    val builder = List.newBuilder[CursorOp]
    while (next.ne(null)) {
      if (next.lastOp.ne(null)) {
        builder += next.lastOp
      }
      next = next.lastCursor
    }
    builder.result()
  }

  def succeeded: Boolean

  final def failed: Boolean = !succeeded

  def right: ACursor

  /*def focus: Option[Step]

  def success: Option[HCursor]

  def top:Option[Step]

  def withFocus(f: Step => Step) :ACursor

  def withFocusM[F[_]](f: Step => F[Step])(implicit F:Applicative[F]):F[ACursor]
 */
  def delete: ACursor

  def downArray: ACursor

  //def downObject: ACursor

  final def as[A](implicit d: Decoder[A], strictness: DecoderStrictness): Decoder.Result[A] = d.tryDecode(this)

}