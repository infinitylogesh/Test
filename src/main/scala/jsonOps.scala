package prime


/**
  * Created by logesh on 26/03/2017.
  */

import io.circe.generic.auto._
import io.circe.syntax._

object jsonOps {

  sealed trait eventsAppQuery

  /* options : case class to construct the options inside the filtered data in the Post query to Events app . This provides the
  *  Further filter level details - Value of the filter. Example - conference (when filter is Event type ) */
  case class options(id:Option[Int],stringId:Option[String],name:String,year:Option[String] = None,
                     selected:Option[Boolean]=None) extends eventsAppQuery

  // TODO : See if this can be combined with options case class and can be defaulted to None.

  /* dateOptions : case class to construct the options exclusively for datarange filter iniside the filtered data in the Post query to Events app*/
  case class dateOptions(startTimeStamp:Option[String],endTimeStamp:Option[String]) extends eventsAppQuery

  /* Case class to filter data - provides the details of the filter name. Example - Event type */
  case class filterData(id:Option[Int],displayName:String,name:String,Type:String,options:Array[options],notDeletable:Option[Boolean]=Some(false),notApplicable:Option[Boolean]=Some(false)) extends eventsAppQuery

  /* Complete query to primeEvents search */
  case class primeEventsJson(query:String,offset:Int=0,limit:Int=10,filterData:Array[filterData],pastEvents:Boolean=false,sortBy:String="relevance") extends eventsAppQuery

  val sample:options = this.options(Some(39),None,"Conference",None,selected = None)
  val filter:filterData = filterData(None,"Event Type","EventType","single",Array(sample),Some(false),Some(false))
  val primeEvent:primeEventsJson = primeEventsJson("@all",0,10,Array(filter),pastEvents = false)


  println(primeEvent.asJson.noSpaces.replace("\"Type\":","\"type:\""))

}
