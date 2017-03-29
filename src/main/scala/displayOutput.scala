package prime

import play.api.libs.concurrent.Execution.Implicits._

/**
  * Created by prime on 28/3/17.
  */
object displayOutput {

  val http = test.http

  def show(outputRequestJson:String):Unit = {

    // TODO: On login complete post request should be sent
    // TODO: Close ws connection.
     val result = http.postRequest(attributes.primeEventsSearchUri,outputRequestJson)
    println(outputRequestJson)
     result.onComplete(completedResult =>{
       if(completedResult.isSuccess){
         println(completedResult.get.body)
          jsonOps.getPrimeEventsListFromJson(completedResult.get.body).map(x=>println(x))

       }else{
         println("Post request failed")
         throw new Exception("Post Request of output failed")
       }
     })
  }

}
