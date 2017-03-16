package prime

/**
  * Created by prime on 16/3/17.
  */

sealed trait Statement;
case class listEvents(eventType:EventType = allEventType,role: Role = allRole,eventCategory: EventCategory = allEventCategory,deduct:Deduct[String] = Deduct("")) extends Statement;

case class Deduct[T](str:T *);

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

