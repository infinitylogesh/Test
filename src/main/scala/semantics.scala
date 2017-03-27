package prime

/**
  * Created by prime on 16/3/17.
  */

sealed trait Statement;

/*
 * This case class holds the definition of all the semantics that are to be mapped to JSON for events app.
 * eventType : Training,meeting,conference,workshops etc
 * role : Participant,Organizer,Creator
 * eventCategory : Account,Project,Global events
 * deduce : Location / Date - Optional argument
 * searchString : A fuzzy string to the backend to restrict the results - Optional argument
 * apps : Events apps - Hub , D&I and iBelong
 */

case class listEvents(eventType:Option[EventType] = None,role: Option[Role] = None,
                      eventCategory: Option[EventCategory] = None,location: Option[Location]=None,date:Option[DateEntity] = None,
                      searchString:Option[SearchString] = None,apps:Option[Apps] = None) extends Statement;

case class run(command:InternalCommands) extends Statement


/*
* Entities that are detected from the sentences
* */

sealed trait Entity
case class Location(value:String) extends Entity

// When date and location are present in single query and Many other attributes are available.
// For conditionality  purposes Date and location are combined
// Ex. Java related events in chennai between 1st and 2nd.
case class DateAndLocation(location: Location,dateEntity: DateEntity)

/*
   case class for different forms of date that can be detected in the sentence.
   DateString will have strings related to dates like week, month , recent , next week etc.
   StartDate will have date start date - YYYY-MM-DD
   EndDate will have date end date - YYYY-MM-DD
   Date Range - Start date and End date.
 */


sealed trait DateEntity extends Entity
case class DateString(value:String) extends DateEntity
case class Date(date:String) extends DateEntity
case class DateRange(startDate: Date,endDate: Date) extends DateEntity

case class SearchString(query:String) extends Entity// Fuzzy search string.

sealed trait Role
case object participant extends Role
case object speaker extends Role
case object organizer extends Role
case object volunteer extends Role
case object allRole extends Role


sealed trait EventType {
  val name : String   // Name to match the string in JSON response from service.
}

case object conference extends EventType {
   val name = "Conference"
}
case object corporate extends EventType{
  val name:String = "Corporate Events"
}
case object training extends EventType{
  val name:String = "Training"
}
case object workshop extends EventType{
  val name:String = "Workshop"
}
case object celebrations extends EventType{
  val name:String = "Celebrations"
}
case object allEventType extends EventType{
  val name:String = ""
}

sealed trait EventCategory {
  val name:String
}
case object account extends EventCategory{
  val name = "Account"
}
case object global extends EventCategory{
  val name = "Global Events"
}
case object group extends EventCategory{
  val name = "Group"
}
case object project extends  EventCategory{
  val name = "Project"
}
case object allEventCategory extends EventCategory{
  val name = ""
}

sealed trait InternalCommands
case object regression extends InternalCommands



sealed trait Apps{
  val name : String
}

case object  DandI extends Apps{
  val name = "D & I"
}
case object  Hubs  extends Apps{
  val name = "Hub"
}
case object  IBelong extends Apps{
  val name = "iBelong"
}


