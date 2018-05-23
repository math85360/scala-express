package com.iz2use.express.ifc.ifc4

import eu.timepit.refined._
import eu.timepit.refined.api._
import eu.timepit.refined.collection._
import utest._
import com.iz2use.express.p21._
import com.iz2use.express.p21.syntax._
import com.iz2use.express.syntax._

object WallTest extends TestSuite {

  val tests = TestSuite {
    implicit val strictness = Strictness.encodeStrictOrConvert
    implicit val db = DatabaseIfc()
    var i = 1
    val building = IfcBuilding(IfcGloballyUniqueId.next, name = IfcLabel("IfcBuilding"))
    db.insert(building)
    'TestIfcBuilding{
      //assert(db.counter == 2)
    }
    val lengthUnit = IfcSIUnit(IfcUnitEnum.LENGTHUNIT, IfcSIPrefix.MILLI, IfcSIUnitName.METRE)
    'IfcSIUnit{
      /*Encoder[IfcUnitEnum]
      Encoder[Option[IfcSIPrefix]]
      Encoder[IfcSIUnitName]
      Encoder[IfcSIUnit]
      val encoder = Encoder[IfcUnit]
      assert(encoder ne null)
      val v = encoder(lengthUnit)
      assert(v == null)*/
      /*assertMatch(v) {
        case StepObject(_, Vector(
      }*/
    }
    /*implicit val uid = new Recoverable[IfcProject, String, NonEmptyWithFixedSize22, Refined] {
      def recover(in: String): Strictness.Result[Refined[String, NonEmptyWithFixedSize22]] = {
        if (in.length > 22) Right(in.take(22))
        else Left("too long !")
      }
    }*/
    val project = IfcProject(IfcGloballyUniqueId.next, name = IfcLabel("IfcProject"),
                             unitsInContext = IfcUnitAssignment(
        Set[IfcUnit](
          lengthUnit)))
    db.insert(project)
    'TestIfcProjectEncoder{
      val encoder = Encoder[IfcProject]
      assert(encoder ne null)
      println(encoder)
      val v = encoder(project)
      assertMatch(v) {
        case StepObject(_, Vector(StepString(id), StepNull, StepString("IfcProject"), StepNull, StepNull, StepNull, StepNull, StepNull, StepReference(unit))) =>
      }
      val valid = """.*'[A-Za-z0-9_$]{22}',\$,'IfcProject',\$,\$,\$,\$,\$,#[0-9]+.*""".r
      assertMatch(v.toString()) {
        case valid() =>
      }
      //assert(db.counter == 5)
      //assert(v == StepObject(
      //val v = IfcProject.encoder(project)
      //assert(v == null)
    }
    val masonryFinish = IfcMaterial("Masonry - Brick - Brown")
    'TestIfcMaterial{
      Encoder[IfcText]
      Encoder[IfcLabel]
      Encoder[Option[IfcText]]
      Encoder[Option[IfcLabel]]
      val encoder = Encoder[IfcMaterial]
      assert(encoder ne null)
      val v = encoder(masonryFinish)
      assertMatch(v) {
        case StepObject(_, Vector(StepString("Masonry - Brick - Brown"), StepNull, StepNull)) =>
      }
    }
    val masonry = IfcMaterial("Masonry")
    val layerFinish = IfcMaterialLayer(masonryFinish, IfcLengthMeasure(110.0), name = IfcLabel("Finish"))
    'TestIfcMaterialLayer{
      Encoder[Option[RefTo[IfcMaterial]]]
      Encoder[IfcNonNegativeLengthMeasure]
      Encoder[Option[IfcLogical]]
      Encoder[Option[IfcInteger]]
      val encoder = Encoder[IfcMaterialLayer]
      assert(encoder ne null)
      val v = encoder(layerFinish)
      assertMatch(v) {
        case StepObject(_, Vector(StepReference(_), StepDouble(110.0), StepNull, StepString("Finish"), StepNull, StepNull, StepNull)) =>
      }
      val valid = """.*#[0-9]+,[0-9.]+,\$,'Finish',\$,\$,\$.*""".r
      assertMatch(v.toString()) {
        case valid() =>
      }
    }
    val airInfiltrationBarrier = IfcMaterialLayer(None, IfcLengthMeasure(50.0), name = IfcLabel("Air Infiltration Barrier"), isVentilated = IfcLogical(True))
    val structure = IfcMaterialLayer(masonry, IfcLengthMeasure(110.0), name = IfcLabel("Core"))
    val name = "Double Brick - 270"
    val materialLayerSet = IfcMaterialLayerSet(List[IfcMaterialLayer](layerFinish, airInfiltrationBarrier, structure), IfcLabel(name))
    val materialLayerSetUsage = IfcMaterialLayerSetUsage(materialLayerSet, IfcLayerSetDirectionEnum.AXIS2, IfcDirectionSenseEnum.POSITIVE, 0)
    val wallType = IfcWallType(IfcGloballyUniqueId.next, name = IfcLabel(name), predefinedType = IfcWallTypeEnum.NOTDEFINED)
    val wallStandardCase = IfcWallStandardCase(
      IfcGloballyUniqueId.next,
      objectPlacement = IfcLocalPlacement(relativePlacement = IfcAxis2Placement3D(IfcCartesianPoint(List(0.0, 0.0, 0.0)))))

    'valid{
      assert(db.counter == 16)
    }
  }
}
