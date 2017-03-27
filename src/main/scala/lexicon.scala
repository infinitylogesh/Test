package prime

import javax.swing.text.html.parser.Entity

import com.workday.montague.ccg.{CcgCat, NP, S, TerminalCat}
import com.workday.montague.parser.{IntegerMatcher, ParserDict}
import com.workday.montague.semantics.{λ, _}
import com.workday.montague.ccg
/**
  * Created by prime on 16/3/17.
  */


// TODO : see if me , my and I to filter to the users role. - my events in chennai

object LexiconOps {

  // Map holding all the eventTypes and its case objects. Used for iteratively apply lexicons for all the eventtypes
  val events2SemanticMap = Map("event"->None,
                              "conference"->Some(conference),
                              "corporate"->Some(corporate),
                              "training"->Some(training),
                              "workshop"->Some(workshop),
                              "celebration"->Some(celebrations));


  // Implicit class to convert all the strings to plural Seq
  implicit class pluralOps(val str:String) extends AnyVal{
    def s:Seq[String] = Seq(str,str+"s");
  }


  var lexicon = ParserDict[CcgCat]() +
              (Seq("that","i have") -> Seq(((NP\NP),identity),((NP/NP),identity),((NP),identity))) +
              (Seq("iam","i am") -> Seq(((NP/V),identity),(NP/NP,identity),((NP\NP)/NP,identity),(NP,identity))) +
              (Seq("participating","participant","a participant","participated","part of") ->(V,Form(Some(participant)):SemanticState)) +
              (Seq("speaking","a speaker","speaker") ->(V,Form(Some(speaker)):SemanticState)) +
              (Seq("organizing","a organizer","organized") ->(V,Form(Some(organizer)):SemanticState)) +
              (Seq("what") ->(NP,identity)) +
              (Seq("me") ->(NP,identity)) +
             // (("between")->()) +
              (Seq("what are all","by") ->Seq(/*(NP/NP,identity),*/(S/NP,identity))) +
              (Seq("by") ->((NP\V)/NP,identity)) +
              (Seq("related") ->Seq(((NP\NP)\NP,identity),((NP\NP),identity),((NP\O),identity))) +
              (Seq("list all the","list the") ->(S/NP,identity)) +
              (Seq("are") ->(NP\NP,identity)) +
              (Seq("and") ->((NP\NP)/NP,λ {d1:Date =>λ {d2:Date =>DateRange(d1,d2)}})) +
              (Seq("happening") ->Seq((NP\NP,identity),
                                      (NP/NP,identity))) +
              (Seq("between") -> Seq((((NP\NP)/NP),λ { dateRange:DateEntity => λ { location:Location => λ { eventType:Option[EventType] => listEvents(eventType,None,None,Some(location),Some(dateRange))}}}),
                                    (((NP\NP)/NP),λ { dateRange:DateEntity =>  λ { eventType:Option[EventType] => listEvents(eventType,None,None,None,Some(dateRange))}}),
                                    (((NP\NP)/NP),λ { dateRange:DateEntity =>  λ { location: Location=> DateAndLocation(location,dateRange)}}),
                                    ((NP/NP),identity)
                                    )) +
              (("run") -> ((NP/I),λ { cmd :InternalCommands =>(run(cmd))})) + // Internal commands
              (("regression") -> (I,Form(regression):SemanticState)) +
              (Seq("the") ->Seq(((NP/NP)\NP,identity),((NP/NP),identity),((NP\NP),identity))) +
              (Seq("my") ->Seq(((S/NP),λ { n3:Option[EventType]  => listEvents(n3,Some(allRole))}),(S/NP,identity),(NP/NP,λ { n3:Option[EventType]  => listEvents(n3,Some(allRole))}),(NP/NP,identity))) +
              (("in")->Seq((NP/NP,identity),((NP\NP)/NP,identity),
                          ((((NP\NP)\NP)/NP),λ{ date:DateEntity=>{ location:Location=>λ{ eventType : Option[EventType] =>λ{ searchString:SearchString =>listEvents(eventType,None,None,Some(location),Some(date),Some(searchString))}}}}),
                          ((NP\V)/NP,λ { location:Location =>λ { role:Option[Role] =>λ { eventType: Option[EventType] => listEvents(eventType,role,None,Some(location))}}}),
                          ((NP\V)/NP,λ { dateEntity: DateEntity =>λ { role:Option[Role] =>λ { eventType: Option[EventType] => listEvents(eventType,role,None,None,Some(dateEntity))}}}))) +
               // TODO : Revisit this approach of using eventsAppsMap.
              (attributeMatcher("apps") -> (A,{app:String=>Form((attributes.eventsAppsMap(app)))})) +
              (attributeMatcher("objects") -> (O,{app:String=>Form(SearchString(app))}))
              // Lexicons for all the event types are injected.
              injectLexiconForEventTypes()


  /*  Injecting lexicon definition for the entities identified from the sentence - Lexicons are intended
  *   to be injected in run time.
  */

  def injectLexicon[T <: Entity](entity:T):Unit = {
      val lexeme:(String,(TerminalCat,SemanticState)) = entity match {
        case d@Location(value:String) => ((value) -> (NP,Form(d):SemanticState))
        case d@DateString(value:String) => ((value) -> (NP,Form(d):SemanticState))
        case d@Date(value:String) => ((value) -> (NP,Form(d):SemanticState))
        case d@SearchString(value:String) => ((value) -> (O,Form(d):SemanticState))
      }
    this.lexicon+=lexeme
  }

  /*
  *  Lexicon for event types - Training,workshop etc are injected using the method. This method is used for
  *  iteratively injecting the same set of lexicons for all the event types.
  *  Note : Update the lexicon for events here.
  * */

  def injectLexiconForEventTypes():Unit={

    events2SemanticMap.foreach(ev => {
      val lexeme = ((ev._1).s -> Seq((NP,Form(ev._2):SemanticState),
        ((S/NP),λ { location: Location  =>  listEvents(ev._2,None,None,Some(location))}), // events organized by me.
        ((S\NP)/NP,λ { location: Location  =>λ { searchString: SearchString  =>  listEvents(ev._2,None,None,Some(location),None,Some(searchString))}}), // events organized by me.
        ((NP/NP),λ { n3:Option[Role]  =>  listEvents(ev._2,n3)}),
        (((S/NP)/V/NP),λ { role:Option[Role]  => λ { dateEntity: DateEntity => listEvents(ev._2,role,None,None,Some(dateEntity))}}),
        ((NP\O),λ { searchString: SearchString  =>  listEvents(ev._2,None,None,None,None,Some(searchString))}), // Ex. java events
        ((NP\A),λ { apps: Apps =>  listEvents(ev._2,None,None,None,None,None,Some(apps))}), // Ex. D&I events
        ((NP/O),λ { searchString: SearchString  =>  listEvents(ev._2,None,None,None,None,Some(searchString))}), // Ex. events related to java , events java
        ((NP/A),λ { apps: Apps  =>  listEvents(ev._2,None,None,None,None,None,Some(apps))}), // Ex. events related to d&i , events d&i
        ((NP/NP),λ {location: Location  =>  listEvents(ev._2,None,None,Some(location))}), // my events in chennai
        (((NP\O)/NP),λ {location: Location =>λ {searchString: SearchString  =>  listEvents(ev._2,None,None,Some(location),None,Some(searchString))}}), // java events in chennai
        (((NP\A)/NP),λ {location: Location =>λ {apps: Apps  =>  listEvents(ev._2,None,None,Some(location),None,None,Some(apps))}}), // d&i events in chennai
        (((NP\O)/NP),λ{ date:DateAndLocation=>λ{searchString: SearchString =>listEvents(ev._2,None,None,Some(date.location),Some(date.dateEntity),Some(searchString))}}), // java events in chennai between 1st and 2nd
        (((NP\A)/NP),λ{ date:DateAndLocation=>λ{apps: Apps =>listEvents(ev._2,None,None,Some(date.location),Some(date.dateEntity),None,Some(apps))}}), // d&i events in chennai between 1st and 2nd
        (((NP\O)/NP),λ {dateEntity: DateEntity =>λ {searchString: SearchString  =>  listEvents(ev._2,None,None,None,Some(dateEntity),Some(searchString))}}), // java events happening this week
        (((NP\A)/NP),λ {dateEntity: DateEntity =>λ {apps: Apps  =>  listEvents(ev._2,None,None,None,Some(dateEntity),None,Some(apps))}}), // d&i events happening this week
        (((NP/NP)/O),λ {searchString: SearchString  =>λ {location: Location =>  listEvents(ev._2,None,None,Some(location),None,Some(searchString))}}), // events related to java in chennai
        (((NP/NP)/A),λ {apps: Apps  =>λ {location: Location =>  listEvents(ev._2,None,None,Some(location),None,None,Some(apps))}}), // events related to java in chennai
        (((NP/NP)/O),λ {searchString: SearchString  =>λ {dateAndLocation: DateAndLocation =>  listEvents(ev._2,None,None,Some(dateAndLocation.location),Some(dateAndLocation.dateEntity),Some(searchString))}}), // events related to java in chennai between 1st and 2nd
        (((NP/NP)/A),λ {apps: Apps  =>λ {dateAndLocation: DateAndLocation =>  listEvents(ev._2,None,None,Some(dateAndLocation.location),Some(dateAndLocation.dateEntity),None,Some(apps))}}), // events related to D&I in chennai between 1st and 2nd
        (((NP/NP)/O),λ {searchString: SearchString  =>λ {dateEntity: DateEntity =>  listEvents(ev._2,None,None,None,Some(dateEntity),Some(searchString))}}), // events related to java this week.
        (((NP/NP)/A),λ {apps: Apps  =>λ {dateEntity: DateEntity =>  listEvents(ev._2,None,None,None,Some(dateEntity),None,Some(apps))}}), // events related to d&i this week.
        ((NP/NP),λ { date:DateEntity  =>  listEvents(ev._2,None,None,None,Some(date))}) // events this week
      ))

      this.lexicon += lexeme
    })

  }

}

