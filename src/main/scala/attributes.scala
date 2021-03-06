/**
  * Created by prime on 23/3/17.
  */
package prime

/*
* Object to hold all the attributes
*/

object attributes {

  // Note : Always maintain attributes in lowercase.
  val eventsApps = Seq("d&i","hub","ibelong")
  // TODO : Revisit this approach
  val eventsAppsMap = Map("d&i"->DandI,"hub"->Hubs,"ibelong"->IBelong)
  val primeObjects = Seq("java","oracle")
  // Map to hold all the attributes with name.
  val attributesMap = Map("apps" -> eventsApps,"objects" -> primeObjects)
  val primeLogInUri = "https://platformprimebeta.ultimatix.net/api/auth/entry"
  val primeEventsSearchUri = "https://platformprimebeta.ultimatix.net/api/search/primeEvents"
  val httpUser = "132291"
  val httpUserPassword = "Platform"

}
