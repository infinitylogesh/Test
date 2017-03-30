package prime

import play.api.libs.concurrent.Execution.Implicits._
import java.util.Date

/**
  * Created by prime on 28/3/17.
  */
object displayOutput {

  val http = test.http

  def show(outputRequestJson:String):Unit = {

    // TODO: On login complete post request should be sent
    // TODO: Close ws connection.
     val result = http.postRequest(attributes.primeEventsSearchUri,outputRequestJson)
    //println(outputRequestJson)
     result.onComplete(completedResult =>{
       if(completedResult.isSuccess){
      //   println(completedResult.get.body)
          if(completedResult.get.body.size==0) println(" 0 Events found")
          jsonOps.getPrimeEventsListFromJson(completedResult.get.body).map(x=>prettyPrint(x))
         println(">>")
       }else{
         println("Post request failed")
         throw new Exception("Post Request of output failed")
       }
     })
  }

  def prettyPrint(event:jsonOps.primeEventList) = {
    //val eventDate = new Date(event.eventDate.getOrElse(0L))
    println("\n"+"id:"+ event.id + "\t" + "City:" + event.city.getOrElse("") + "\t" + "Country:" + event.country.getOrElse("") /*+ "\t" + "Event date:" + eventDate*/)
    println("Title:"+ event.title.getOrElse(""))
    println("Description:" + event.description.getOrElse(""))
    println("------------------------------------------------------------------------------------------------------------------")
  }

}
