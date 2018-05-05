package com.iz2use.express.ast

sealed trait AddLikeOp extends Operator
case object AdditionOp extends AddLikeOp
case object SubtractionOp extends AddLikeOp
case object OrOp extends AddLikeOp
case object XorOp extends AddLikeOp

sealed trait MultiplicationLikeOp extends Operator
case object MultiplicationOp extends MultiplicationLikeOp
case object DivisionOp extends MultiplicationLikeOp
case object ModuloOp extends MultiplicationLikeOp
case object AndOp extends MultiplicationLikeOp
case object OrElseOp extends MultiplicationLikeOp

sealed trait Operator
sealed trait RelOpExtended extends Operator
sealed trait RelOp extends RelOpExtended
case object LT extends RelOp
case object GT extends RelOp
case object LE extends RelOp
case object GE extends RelOp
case object NE extends RelOp
case object EQ extends RelOp
case object TE extends RelOp
case object TN extends RelOp
case object IN extends RelOpExtended
case object LIKE extends RelOpExtended

sealed trait UnaryOp extends Operator
case object PositiveOp extends UnaryOp
case object NegativeOp extends UnaryOp
case object NotOp extends UnaryOp

case object PowerOp extends Operator