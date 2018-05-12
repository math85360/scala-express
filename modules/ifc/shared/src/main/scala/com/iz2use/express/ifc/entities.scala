package com.iz2use.express.ifc

import eu.timepit.refined._
import eu.timepit.refined.api.Refined
import eu.timepit.refined.auto._
import eu.timepit.refined.collection._
import eu.timepit.refined.numeric._
//import com.iz2use.express.step._
import shapeless.Coproduct

trait StepEntity { self =>
  protected[ifc] def db: DatabaseIfc
  db.insert(self)
}
trait IfcRoot extends StepEntity {
  def globalId: IfcGloballyUniqueId
  def ownerHistory: Option[RefTo[IfcOwnerHistory]]
  def name: Option[IfcLabel]
  def description: Option[IfcText]
}
case class IfcOwnerHistory(
  owningUser:               IfcPersonAndOrganization,
  owningApplication:        IfcApplication,
  state:                    Option[IfcStateEnum],
  changeAction:             Option[IfcChangeActionEnum],
  lastModifiedDate:         Option[IfcTimeStamp],
  lastModifyingUser:        Option[IfcPersonAndOrganization],
  lastModifyingApplication: Option[IfcApplication],
  creationDate:             IfcTimeStamp
)(implicit override protected[ifc] val db: DatabaseIfc) extends StepEntity
case class IfcPersonAndOrganization(
  thePerson:       IfcPerson,
  theOrganization: IfcOrganization,
  roles:           Option[List[IfcActorRole] Refined NonEmpty]
)(implicit override protected[ifc] val db: DatabaseIfc) extends StepEntity
case class IfcPerson()
case class IfcOrganization()
case class IfcActorRole()
case class IfcApplication(
  applicationDeveloper:  IfcOrganization,
  version:               IfcLabel,
  applicationFullName:   IfcLabel,
  applicationIdentifier: IfcIdentifier
)(implicit override protected[ifc] val db: DatabaseIfc) extends StepEntity
trait IfcObjectDefinition extends IfcRoot {

}
trait IfcObject extends IfcObjectDefinition {
  def objectType: Option[IfcLabel]
}
sealed trait IfcObjectPlacement
case class IfcLocalPlacement(
  placementRelTo:    Option[IfcObjectPlacement] = None,
  relativePlacement: IfcAxis2Placement
) extends IfcObjectPlacement
trait IfcRepresentation {
}
trait IfcProductRepresentation {
  def name: Option[IfcLabel]
  def description: Option[IfcText]
  def representations: Option[List[IfcRepresentation] Refined NonEmpty]
}
trait IfcProduct extends IfcObject {
  def objectPlacement: Option[RefTo[IfcObjectPlacement]]
  def representation: Option[RefTo[IfcProductRepresentation]]
}
trait IfcSpatialElement extends IfcProduct {
  def longName: Option[IfcLabel]
}
sealed trait IfcChangeActionEnum
object IfcChangeActionEnum {
  case object NoChange extends IfcChangeActionEnum
  case object Modified extends IfcChangeActionEnum
  case object Added extends IfcChangeActionEnum
  case object Deleted extends IfcChangeActionEnum
  case object NotDefined extends IfcChangeActionEnum
}
sealed trait IfcStateEnum
object IfcStateEnum {
  case object ReadWrite extends IfcStateEnum
  case object ReadOnly extends IfcStateEnum
  case object Locked extends IfcStateEnum
  case object ReadWriteLocked extends IfcStateEnum
  case object ReadOnlyLocked extends IfcStateEnum
}
sealed trait IfcElementCompositionEnum
object IfcElementCompositionEnum {
  case object Complex extends IfcElementCompositionEnum
  case object Element extends IfcElementCompositionEnum
  case object Partial extends IfcElementCompositionEnum
}
trait IfcSpatialStructureElement extends IfcSpatialElement {
  def compositionType: Option[IfcElementCompositionEnum]
}
case class IfcBuilding(
    globalId:             IfcGloballyUniqueId,
    ownerHistory:         Option[RefTo[IfcOwnerHistory]]          = None,
    name:                 Option[IfcLabel]                        = None,
    description:          Option[IfcText]                         = None,
    objectType:           Option[IfcLabel]                        = None,
    objectPlacement:      Option[RefTo[IfcObjectPlacement]]       = None,
    representation:       Option[RefTo[IfcProductRepresentation]] = None,
    longName:             Option[IfcLabel]                        = None,
    compositionType:      Option[IfcElementCompositionEnum]       = None,
    elevationOfRefHeight: Option[IfcLengthMeasure]                = None,
    elevationOfTerrain:   Option[IfcLengthMeasure]                = None,
    buildingAddress:      Option[RefTo[IfcPostalAddress]]         = None
)(implicit override protected[ifc] val db: DatabaseIfc) extends IfcSpatialStructureElement {

}

case class IfcPostalAddress()
trait IfcRepresentationContext
sealed trait IfcUnit extends StepEntity
object IfcUnit {
  //implicit val encoder 
}
case class IfcMonetaryUnit(currency: IfcLabel)(implicit override protected[ifc] val db: DatabaseIfc) extends IfcUnit
sealed trait IfcNamedUnit extends IfcUnit {
  def dimensions: RefTo[IfcDimensionalExponents]
  def unitType: IfcUnitEnum
}
sealed abstract class IfcSIPrefix(val exponent: Int)
object IfcSIPrefix {
  case object Exa extends IfcSIPrefix(18)
  case object Peta extends IfcSIPrefix(15)
  case object Tera extends IfcSIPrefix(12)
  case object Giga extends IfcSIPrefix(9)
  case object Mega extends IfcSIPrefix(6)
  case object Kilo extends IfcSIPrefix(3)
  case object Hecto extends IfcSIPrefix(2)
  case object Deca extends IfcSIPrefix(1)
  case object Deci extends IfcSIPrefix(-1)
  case object Centi extends IfcSIPrefix(-2)
  case object Milli extends IfcSIPrefix(-3)
  case object Micro extends IfcSIPrefix(-6)
  case object Nano extends IfcSIPrefix(-9)
  case object Pico extends IfcSIPrefix(-12)
  case object Femto extends IfcSIPrefix(-15)
  case object Atto extends IfcSIPrefix(-18)
}
sealed abstract class IfcSIUnitName {
}
object IfcSIUnitName {
  case object Ampere extends IfcSIUnitName
  case object Becquerel extends IfcSIUnitName
  case object Candela extends IfcSIUnitName
  case object Coulomb extends IfcSIUnitName
  case object Cubic_metre extends IfcSIUnitName
  case object Degree_celsius extends IfcSIUnitName
  case object Farad extends IfcSIUnitName
  case object Gram extends IfcSIUnitName
  case object Gray extends IfcSIUnitName
  case object Henry extends IfcSIUnitName
  case object Jertz extends IfcSIUnitName
  case object Joule extends IfcSIUnitName
  case object Kelvin extends IfcSIUnitName
  case object Lumen extends IfcSIUnitName
  case object Lux extends IfcSIUnitName
  case object Metre extends IfcSIUnitName
  case object Mole extends IfcSIUnitName
  case object Newton extends IfcSIUnitName
  case object Ohm extends IfcSIUnitName
  case object Pascal extends IfcSIUnitName
  case object Radian extends IfcSIUnitName
  case object Second extends IfcSIUnitName
  case object Siemens extends IfcSIUnitName
  case object Sievert extends IfcSIUnitName
  case object SquareMetre extends IfcSIUnitName
  case object Steradian extends IfcSIUnitName
  case object Tesla extends IfcSIUnitName
  case object Volt extends IfcSIUnitName
  case object Watt extends IfcSIUnitName
  case object Weber extends IfcSIUnitName
}
case class IfcSIUnit(
    //dimensions: RefTo[IfcDimensionalExponents],
    unitType: IfcUnitEnum,
    prefix:   Option[IfcSIPrefix],
    name:     IfcSIUnitName
)(implicit override protected[ifc] val db: DatabaseIfc) extends IfcNamedUnit {
  val dimensions = RefToDerived()
}
sealed abstract class IfcUnitEnum
object IfcUnitEnum {
  case object AbsorbedDoseUnit extends IfcUnitEnum
  case object AmountOfSubstanceUnit extends IfcUnitEnum
  case object AreaUnit extends IfcUnitEnum
  case object DoseEquivalentUnit extends IfcUnitEnum
  case object ElectricCapacitanceUnit extends IfcUnitEnum
  case object ElectricChargeUnit extends IfcUnitEnum
  case object ElectricConductanceUnit extends IfcUnitEnum
  case object ElectricCurrentUnit extends IfcUnitEnum
  case object ElectricResistanceUnit extends IfcUnitEnum
  case object ElectricVoltageUnit extends IfcUnitEnum
  case object EnergyUnit extends IfcUnitEnum
  case object ForceUnit extends IfcUnitEnum
  case object FrequencyUnit extends IfcUnitEnum
  case object IlluminanceUnit extends IfcUnitEnum
  case object InductanceUnit extends IfcUnitEnum
  case object LengthUnit extends IfcUnitEnum
  case object LuminousFluxUnit extends IfcUnitEnum
  case object LuminousIntensityUnit extends IfcUnitEnum
  case object MagneticFluxDensityUnit extends IfcUnitEnum
  case object MagneticFluxUnit extends IfcUnitEnum
  case object MassUnit extends IfcUnitEnum
  case object PlaneAngleUnit extends IfcUnitEnum
  case object PowerUnit extends IfcUnitEnum
  case object PressureUnit extends IfcUnitEnum
  case object RadioActivityUnit extends IfcUnitEnum
  case object SolidAngleUnit extends IfcUnitEnum
  case object ThermodynamicTemperatureUnit extends IfcUnitEnum
  case object TimeUnit extends IfcUnitEnum
  case object VolumeUnit extends IfcUnitEnum
  case object UserDefined extends IfcUnitEnum
}
case class IfcDimensionalExponents(
  LengthExponent:                   Int,
  MassExponent:                     Int,
  TimeExponent:                     Int,
  ElectricCurrentExponent:          Int,
  ThermodynamicTemperatureExponent: Int,
  AmountOfSubstanceExponent:        Int,
  LuminousIntensityExponent:        Int
)
case class IfcUnitAssignment(units: Set[RefTo[IfcUnit]] Refined NonEmpty)(implicit override protected[ifc] val db: DatabaseIfc) extends StepEntity
trait IfcContext extends IfcObjectDefinition {
  def objectType: Option[IfcLabel]
  def longName: Option[IfcLabel]
  def phase: Option[IfcLabel]
  def representationContexts: Option[Set[RefTo[IfcRepresentationContext]] Refined NonEmpty]
  def unitsInContext: Option[RefTo[IfcUnitAssignment]]
}
case class IfcProject(
  globalId:               IfcGloballyUniqueId,
  ownerHistory:           Option[RefTo[IfcOwnerHistory]]                                = None,
  name:                   Option[IfcLabel]                                              = None,
  description:            Option[IfcText]                                               = None,
  objectType:             Option[IfcLabel]                                              = None,
  longName:               Option[IfcLabel]                                              = None,
  phase:                  Option[IfcLabel]                                              = None,
  representationContexts: Option[Set[RefTo[IfcRepresentationContext]] Refined NonEmpty] = None,
  unitsInContext:         Option[RefTo[IfcUnitAssignment]]                              = None
)(implicit override protected[ifc] val db: DatabaseIfc) extends IfcContext
object IfcProject {
  //implicit val encoder = Encoder[IfcProject]
  //implicit val decoder: Decoder[IfcProject] = deriveDecoder
}
sealed trait IfcMaterialDefinition
case class IfcMaterial(
  name:        IfcLabel,
  description: Option[IfcText]  = None,
  category:    Option[IfcLabel] = None
) extends IfcMaterialDefinition
object IfcMaterial {
  //implicit val encoder = Encoder[IfcMaterial]
}
case class IfcMaterialLayer(
  material:       Option[RefTo[IfcMaterial]],
  layerThickness: IfcNonNegativeLengthMeasure, // MUST BE POSITIVE
  isVentilated:   Option[IfcLogical]          = None,
  name:           Option[IfcLabel]            = None,
  description:    Option[IfcText]             = None,
  category:       Option[IfcLabel]            = None,
  priority:       Option[IfcInteger]          = None
) extends IfcMaterialDefinition
case class IfcMaterialLayerSet(
  materialLayers: List[RefTo[IfcMaterialLayer]] Refined NonEmpty,
  layerSetName:   Option[IfcLabel]                               = None,
  description:    Option[IfcText]                                = None
) extends IfcMaterialDefinition
sealed trait IfcMaterialUsageDefinition
sealed abstract class IfcLayerSetDirectionEnum(index: Int)
object IfcLayerSetDirectionEnum {
  case object Axis1 extends IfcLayerSetDirectionEnum(0)
  case object Axis2 extends IfcLayerSetDirectionEnum(1)
  case object Axis3 extends IfcLayerSetDirectionEnum(2)
}
sealed abstract class IfcDirectionSenseEnum(direction: Int)
object IfcDirectionSenseEnum {
  case object Positive extends IfcDirectionSenseEnum(1)
  case object Negative extends IfcDirectionSenseEnum(-1)
}
case class IfcMaterialLayerSetUsage(
  forLayerSet:             RefTo[IfcMaterialLayerSet],
  layerSetDirection:       IfcLayerSetDirectionEnum,
  directionSense:          IfcDirectionSenseEnum,
  offsetFromReferenceLine: IfcLengthMeasure,
  referenceExtent:         Option[IfcPositiveLengthMeasure] = None
) extends IfcMaterialUsageDefinition
trait IfcTypeObject extends IfcObjectDefinition {
  def applicableOccurrence: Option[IfcIdentifier]
  def hasPropertySets: Option[Set[IfcPropertySetDefinition] Refined NonEmpty]
}
trait IfcTypeProduct extends IfcTypeObject {
  def representationMaps: Option[List[IfcRepresentationMap] Refined NonEmpty]
  def tag: Option[IfcLabel]
}
trait IfcElementType extends IfcTypeProduct {
  def elementType: Option[IfcLabel]
}
sealed trait IfcWallTypeEnum
object IfcWallTypeEnum {
  case object Movable extends IfcWallTypeEnum
  case object Parapet extends IfcWallTypeEnum
  case object Partitioning extends IfcWallTypeEnum
  case object PlumbingWall extends IfcWallTypeEnum
  case object Shear extends IfcWallTypeEnum
  case object SolidWall extends IfcWallTypeEnum
  case object Standard extends IfcWallTypeEnum
  case object Polygonal extends IfcWallTypeEnum
  case object ElementedWall extends IfcWallTypeEnum
  case object UserDefined extends IfcWallTypeEnum
  case object NotDefined extends IfcWallTypeEnum
}
trait IfcBuildingElementType extends IfcElementType
trait IfcPropertyDefinition extends IfcRoot
trait IfcPropertySetDefinition extends IfcPropertyDefinition
trait IfcRepresentationItem extends StepEntity
trait IfcGeometricRepresentationItem extends IfcRepresentationItem
trait IfcPlacement extends IfcGeometricRepresentationItem {
  def location: RefTo[IfcCartesianPoint]
}
case class IfcDirection(
  directionRatios: List[IfcReal] Refined SizeBetween2And3
)(implicit override protected[ifc] val db: DatabaseIfc) extends IfcGeometricRepresentationItem
case class IfcAxis2Placement2D(
  location:     RefTo[IfcCartesianPoint],
  refDirection: Option[IfcDirection]     = None
)(implicit override protected[ifc] val db: DatabaseIfc) extends IfcPlacement
object IfcAxis2Placement2D {
  implicit def toIfcAxis2Placement(x: IfcAxis2Placement2D) = Coproduct[IfcAxis2Placement](x)
}
case class IfcAxis2Placement3D(
  location:     RefTo[IfcCartesianPoint],
  axis:         Option[IfcDirection]     = None,
  refDirection: Option[IfcDirection]     = None
)(implicit override protected[ifc] val db: DatabaseIfc) extends IfcPlacement
object IfcAxis2Placement3D {
  implicit def toIfcAxis2Placement(x: IfcAxis2Placement3D) = Coproduct[IfcAxis2Placement](x)
}
case class IfcRepresentationMap(
  mappingOrigin:        IfcAxis2Placement,
  mappedRepresentation: IfcRepresentation
)
case class IfcWallType(
  globalId:             IfcGloballyUniqueId,
  ownerHistory:         Option[RefTo[IfcOwnerHistory]]                         = None,
  name:                 Option[IfcLabel]                                       = None,
  description:          Option[IfcText]                                        = None,
  applicableOccurrence: Option[IfcIdentifier]                                  = None,
  hasPropertySets:      Option[Set[IfcPropertySetDefinition] Refined NonEmpty] = None,
  representationMaps:   Option[List[IfcRepresentationMap] Refined NonEmpty]    = None,
  tag:                  Option[IfcLabel]                                       = None,
  elementType:          Option[IfcLabel]                                       = None,
  predefinedType:       IfcWallTypeEnum
)(implicit override protected[ifc] val db: DatabaseIfc) extends IfcBuildingElementType

trait IfcElement extends IfcProduct {
  def tag: Option[IfcIdentifier]
}
trait IfcBuildingElement extends IfcElement {

}
abstract class IfcWall extends IfcBuildingElement {
  def predefinedType: Option[IfcWallTypeEnum]
}
case class IfcWallStandardCase(
  globalId:        IfcGloballyUniqueId,
  ownerHistory:    Option[RefTo[IfcOwnerHistory]]          = None,
  name:            Option[IfcLabel]                        = None,
  description:     Option[IfcText]                         = None,
  objectType:      Option[IfcLabel]                        = None,
  objectPlacement: Option[RefTo[IfcObjectPlacement]]       = None,
  representation:  Option[RefTo[IfcProductRepresentation]] = None,
  tag:             Option[IfcIdentifier]                   = None,
  predefinedType:  Option[IfcWallTypeEnum]                 = None
)(implicit override protected[ifc] val db: DatabaseIfc) extends IfcWall
trait IfcPoint extends IfcGeometricRepresentationItem
case class IfcCartesianPoint(
  Coordinates: List[IfcLengthMeasure] Refined SizeBetween1And3
)(implicit override protected[ifc] val db: DatabaseIfc) extends IfcPoint
case class IfcNormalisedRatioMeasure()
