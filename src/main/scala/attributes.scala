/**
  * Created by prime on 23/3/17.
  */
package prime

import io.circe.generic.auto._
import io.circe.syntax._

/*
* Object to hold all the attributes
*/

object attributes {

  // TODO : See if this needs to be moduled.

  case class options(id:Option[Int],stringId:Option[String],name:String,year:Option[String] = None,
                     selected:Option[Boolean]=None)

  case class dateOptions(startTimeStamp:Option[String],endTimeStamp:Option[String]);

  case class filterData(id:Option[Int],displayName:String,name:String,Type:String,options:Array[options],notDeletable:Option[Boolean]=Some(false),notApplicable:Option[Boolean]=Some(false))

  case class primeEventsJson(query:String,offset:Int=0,limit:Int=10,filterData:Array[filterData],pastEvents:Boolean=false,sortBy:String="relevance");


  val sample = options(Some(39),None,"Conference",None,selected = None)
  val filter = filterData(None,"Event Type","EventType","single",Array(sample),Some(false),Some(false))
  val primeEvent = primeEventsJson("@all",0,10,Array(filter),false,"relevance")

  //  TODO END

  println(primeEvent.asJson.noSpaces)


  // Note : Always maintain attributes in lowercase.
  val eventsApps = Seq("d&i","hub","ibelong")
  // TODO : Revisit this approach
  val eventsAppsMap = Map("d&i"->DandI,"hub"->Hubs,"ibelong"->IBelong)
  val primeObjects = Seq("java","oracle")
  // Map to hold all the attributes with name.
  val attributesMap = Map("apps" -> eventsApps,"objects" -> primeObjects)
}
