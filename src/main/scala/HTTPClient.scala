/**
  * Created by prime on 28/3/17.
  */

package prime

import com.ning.http.client.AsyncHttpClientConfig
import play.api.libs.ws.ning._
import play.api.libs.ws._
import play.api.libs.json._

import scala.concurrent.Future
import play.api.libs.concurrent.Execution.Implicits._
import java.util.Calendar
import scala.util.{Success,Failure}

class HTTPClient {

  private var ws:NingWSClient = NingWSClient()
  private var jSessionID:String = ""

  /* TODO : to be removed / Refactored to handle login */
  private var user:String = ""
  private var password:String = ""
  private var lastLoggedInTime:BigInt = 0L
  var isLoggedIn = false

  /*
  * Initializes the play ws standalone client with default ningAsyncHttpClient
  *  Proxy details are also initialized. Sets the ws variable to WSClient.
  * */

  def init() = {
    // Properties to set proxy.
    System.setProperty("http.proxyHost","proxy.tcs.com")
    System.setProperty("http.proxyPort","8080")

    val config = new NingAsyncHttpClientConfigBuilder(NingWSClientConfig()).build
    val builder = new AsyncHttpClientConfig.Builder(config)
    ws = new NingWSClient(builder.build)
  }


  /* A wrapper for the WS get requests and returns WSResponse as future */
  def getRequest(uri:String) : Future[WSResponse] = {
    if(jSessionID.nonEmpty && user.nonEmpty){
      val headersData = "Cookie" -> s"JSESSIONID=$jSessionID;CSM_USER=$user"
      ws.url(uri).withHeaders(headersData).get
    }else{
      throw new Exception("Get Request - Session ID and User ID is not set")
    }
  }

  /* A wrapper for the Post get requests and returns WSResponse as future
  *  Uri and JSON body is passed as string
  * */
  def postRequest(uri:String,jsonString:String) : Future[WSResponse] = {

    if(isLoggedIn){
      var headersData = "Cookie" -> s"JSESSIONID=$jSessionID;CSM_USER=$user"
      ws.url(uri).withHeaders(headersData).post(Json.parse(jsonString))
    }else{ // in case of login Post request
      ws.url(uri).post(Json.parse(jsonString))
    }
  }

  // SessionID is extracted from the Cookie and set in the private SessionID variable.
  private def setSessionID(sessionString:WSCookie) = {
      jSessionID = sessionString.value.get.toString  //TODO: See if the get should be changed to getOrElse
  }

  /*
  *  Gets the user and password and logs into the prime service using Post request and sets the sessionID
  *  if the response is successful.
  * */
  @throws(classOf[Exception])
  def primeLogIn(user:String = user,password:String = password) = {
    val authJson = s"""{"employeeAuth":{"employeeNumber":"$user","password":"$password"}}"""
   // if(getTimeStamp - lastLoggedInTime > 300000) { // if last log in is more than 5 mins.
      val wsResponse = postRequest(attributes.primeLogInUri, authJson)
      wsResponse.onComplete{
        case Success(response) =>{
          if (response.validateSuccess) {
            setSessionID(response.cookie("JSESSIONID").get)
            isLoggedIn = true
            //lastLoggedInTime = getTimeStamp
            println("Log: Logged In")
          } else {
            throw new Exception("Log in to prime failed")
          }
        }
        case Failure(t) => throw new Exception("Log in to prime failed" + t.getMessage)
      }
  }

  def getTimeStamp:BigInt = Calendar.getInstance().getTimeInMillis

  // implicit function to check if wsResponse is successful.
  implicit class wsResponseOps(response:WSResponse){
    def validateSuccess = (response.status == 200)
    def unauthorized = (response.status == 401)
  }

}

object HTTPClient{

  def apply() = {
    val hTTPClient = new HTTPClient();
      hTTPClient.user = attributes.httpUser
      hTTPClient.password = attributes.httpUserPassword
      hTTPClient.init()
      hTTPClient.primeLogIn()
      hTTPClient
  }

}


