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

case class listEvents(eventType:EventType = allEventType,role: Role = allRole,
                      eventCategory: EventCategory = allEventCategory,location: Option[Location]=None,date:Option[DateEntity] = None,
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

sealed trait EventType
case object conference extends EventType
case object corporate extends EventType
case object training extends EventType
case object workshop extends EventType
case object celebrations extends EventType
case object allEventType extends EventType

sealed trait EventCategory
case object account extends EventCategory
case object global extends EventCategory
case object group extends EventCategory
case object project extends  EventCategory
case object allEventCategory extends EventCategory

sealed trait InternalCommands
case object regression extends InternalCommands

sealed trait Apps
case object  DandI extends Apps
case object  Hubs  extends Apps
case object  IBelong extends Apps


