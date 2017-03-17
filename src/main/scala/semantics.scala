package prime

/**
  * Created by prime on 16/3/17.
  */

sealed trait Statement;

/*
 * This case class holds the definition of all the semantics that are to be mapped to JSON for events app.
 * eventType : Training,meeting,conference,workshops etc
 * role : Participant,Organizer,Creater
 * eventCategory : Account,Project,Global events
 * deduce : Location / Date - Optional argument TODO : see if date should be handled separately
 * searchString : A fuzzy string to the backend to restrict the results - Optional argument
 */

case class listEvents(eventType:EventType = allEventType,role: Role = allRole,
                      eventCategory: EventCategory = allEventCategory,deduce:Option[Entity]=None,
                      searchString:Option[SearchString] = None) extends Statement;


/*
* Entities that are detected from the sentences
* */

sealed trait Entity
case class Location(value:String) extends Entity
case class Date(value:String) extends Entity

case class SearchString(query:String) // Fuzzy search string.

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
case object allEventType extends EventType

sealed trait EventCategory
case object account extends EventCategory
case object global extends EventCategory
case object group extends EventCategory
case object project extends  EventCategory
case object allEventCategory extends EventCategory

