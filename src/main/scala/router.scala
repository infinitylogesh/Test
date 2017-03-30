package prime

import com.workday.montague.ccg.CcgCat
import com.workday.montague.parser.SemanticParseNode
import com.workday.montague.semantics.Form
import com.workday.montague.semantics.SemanticState
import java.util.Date
import java.text.SimpleDateFormat

/**
  * Created by prime on 21/3/17.
  */
object router {

  var completeFilterData:Array[jsonOps.filterData] = Array()
  var query = "@all" // Default query
  var primeEventsDefaultCaseClass:jsonOps.primeEventsJson = jsonOps.primeEventsJson(query,0,10,Array(),false)
  var primeEventsFinalCaseClass:jsonOps.primeEventsJson = primeEventsDefaultCaseClass

  /*
  *  routes to the required functions based on the Semantic state.
  * */
  def route(output:Option[SemanticParseNode[CcgCat]]):Unit = {
    val semanticState = output.map(_.semantic)
    semanticState match {
      case Some(Form(run(x))) => x match {  // in case of run case class -> routes to run the internal command.
        case regression:regression.type => regress.renderResult();
      }
      case Some(Form(d@listEvents(_,_,_,_,_,_,_))) => processListEvents(d)
      case _ => println(output); // TODO : Remove after usage
    }
  }


  /* Function to reset variables after a JSON request is generated */

  def resetVariables():Unit = {
    completeFilterData = Array()
    query = "@all"
    primeEventsDefaultCaseClass = jsonOps.primeEventsJson(query,0,10,Array(),false)
    primeEventsFinalCaseClass = primeEventsDefaultCaseClass
  }

  /* Final query is composed using the query public query variable and final filterdata  array accumulated from case class*/
  def composeFinalQuery():Unit = {
    if(completeFilterData.size == 0){ // when no filterdata is formed - default date range filter is applied for 1 month
      val currentMonth = new SimpleDateFormat("yyyy-MM").format(new Date())
      val defaultTimeperiod = dateOps.getDateRange(currentMonth).get
      val newFilterData = getFilterDataForDateRange(defaultTimeperiod._1,defaultTimeperiod._2)
      updateFilterDataArray(newFilterData)
    }
    primeEventsFinalCaseClass = primeEventsDefaultCaseClass.copy(query=query,filterData=completeFilterData);
    displayOutput.show(jsonOps.convertToJson(primeEventsFinalCaseClass))
    resetVariables()
  }

/*
    JSON case classes are filtered using the option name (name present in the options case class ) and filter data name
    (name of the filter Data.)
    TODO : Remove - Sorty By is done to get the cities case class with maximum option size, As the cities is being added twice in the case class array.
*/
  def getFilterDataFromJson(filterName:String,optionName:String):jsonOps.filterData = {
    val filterData = jsonOps.parsedJson.filter(x=>x.name.toLowerCase==filterName.toLowerCase).sortBy(- _.options.size).head
    filterData.copy(options = filterData.options.filter(x=>x.name.get.toLowerCase == optionName.toLowerCase)) // TODO : Optimize filter for cities
  }

  def getFilterDataForDateRange(startTimeStamp:Long,endTimeStamp:Long):jsonOps.filterData = {
    val options = jsonOps.options(None,None,None,None,None,Some(startTimeStamp),Some(endTimeStamp))
    jsonOps.filterData(None,"Date Range","Date Range","enddate",Array(options),Some(false),Some(false))
  }


  def processListEvents(parsedOutput : listEvents):Unit = {

    println("Parsed value: " + parsedOutput)

    /* Case class is converted to a iterator and each fields are processed one by one */
    val listEventsFieldsItr = parsedOutput.productIterator
    for(field <- listEventsFieldsItr){
      field match {
        case Some(eventType:EventType) => {
            val filterName = eventType.getClass.getInterfaces.apply(0).getSimpleName
            val newFilterData = getFilterDataFromJson(filterName,eventType.name)
            updateFilterDataArray(newFilterData)
        }

        case Some(eventCategory :EventCategory) => Nil //TODO : Update
        case Some(role:Role) => Nil
        case Some(location:Location) => {

          // TODO : Stub for including region. Needs to be changed.
          val newFilterData2 = getFilterDataFromJson("Region","India")
          updateFilterDataArray(newFilterData2)

          val newFilterData = getFilterDataFromJson(location.name,location.value)
          updateFilterDataArray(newFilterData)
        }
        case Some(d@DateString(date)) => {
          val newFilterData = getFilterDataForDateRange(d.dateValue.get._1,d.dateValue.get._2)
          updateFilterDataArray(newFilterData)
        }
        case Some(d@DateRange(date1,date2)) => {
         val newFilterData = getFilterDataForDateRange(date1.dateValue.get._1,date2.dateValue.get._2)
          updateFilterDataArray(newFilterData)
        }
        case Some(SearchString(query)) =>{
            this.query = query
        }
        case Some(app:Apps) => {
          val filterName = app.getClass.getInterfaces.apply(0).getSimpleName
          val newFilterData = getFilterDataFromJson(filterName,app.name)
          updateFilterDataArray(newFilterData)
        }
        case None => Nil
        case _ => Nil
      }
    }
    composeFinalQuery()
  }

  // filterData case classes are accumulated in completeFilterData array.
  def updateFilterDataArray(fd:jsonOps.filterData):Array[jsonOps.filterData] = {
    completeFilterData = (completeFilterData :+ fd)
    completeFilterData
  }

}
