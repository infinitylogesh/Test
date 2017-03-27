package prime

import com.workday.montague.ccg.CcgCat
import com.workday.montague.parser.SemanticParseNode
import com.workday.montague.semantics.Form
import com.workday.montague.semantics.SemanticState

/**
  * Created by prime on 21/3/17.
  */
object router {

  var completeFilterData:Array[jsonOps.filterData] = Array()
  var query = "@all" // Default query
  var primeEventsFinalQuery:jsonOps.primeEventsJson = jsonOps.primeEventsJson(query,0,10,Array(),false)

  /*
  *  routes to the required functions based on the Semantic state.
  * */
  def route(output:Option[SemanticParseNode[CcgCat]]) = {
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

  def resetVariables() = {
    completeFilterData = Array()
    query = "@all"
    primeEventsFinalQuery = jsonOps.primeEventsJson(query,0,10,Array(),false)
  }

  /* Final query is composed using the query public query variable and final filterdata  array accumulated from case class*/
  def composeFinalQuery() = {
    primeEventsFinalQuery = primeEventsFinalQuery.copy(query=query,filterData=completeFilterData);
    println(jsonOps.convertToJson(primeEventsFinalQuery))
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


  def processListEvents(parsedOutput : listEvents) = {

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
        case Some(date:Date) => Nil
        case Some(SearchString(query)) =>{
            this.query = query
        }
        case Some(app:Apps) => {
          val filterName = app.getClass.getInterfaces.apply(0).getSimpleName
          val newFilterData = getFilterDataFromJson(filterName,app.name)
          updateFilterDataArray(newFilterData)
        }
        case None => Nil
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
