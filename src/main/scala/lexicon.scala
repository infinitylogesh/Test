package prime

import javax.swing.text.html.parser.Entity

import com.workday.montague.ccg.{CcgCat, NP, S}
import com.workday.montague.parser.{IntegerMatcher, ParserDict}
import com.workday.montague.semantics.{Form, SemanticState, identity, λ}
import com.workday.montague.ccg
/**
  * Created by prime on 16/3/17.
  */

//TODO : events in chennai between 1st and 2nd
// TODO : java events in chennai

object LexiconOps {

  implicit class pluralOps(val str:String) extends AnyVal{
    def s:Seq[String] = Seq(str,str+"s");
  }

  var lexicon = ParserDict[CcgCat]() +
    (("event".s) -> Seq((NP,Form(allEventType):SemanticState),
                        ((S/NP),λ { n3:Role  =>  listEvents(allEventType,n3)}), // events organized by me.
                        ((S/NP),λ { date:DateString  =>  listEvents(allEventType,allRole,allEventCategory,None,Some(date))}), // events this week
                        ((S\O),λ { searchString: SearchString  =>  listEvents(allEventType,allRole,allEventCategory,None,None,Some(searchString))}), // Java events , Java related events
                     // TODO:
                        ((NP\O),λ { searchString: SearchString => λ { location: Location => listEvents(allEventType,allRole,allEventCategory,Some(location),None,Some(searchString))}}) // Java related events happening in this week
                        )) +
    (("conference".s) -> (NP,Form(conference):SemanticState)) +
    (Seq("that") -> ((NP\NP),identity)) +
    (Seq("iam","i am") ->Seq(((S/V)\NP,λ { n3:EventType  => λ { n2:Role => listEvents(n3,n2)}}),
                            (((S/NP)/V)\NP,λ { eventType:EventType  => λ { role :Role =>λ { location:Location=>listEvents(eventType,role,allEventCategory,Some(location))}}}))) + // events i am participating in chennai.
    (Seq("participating","participant","a participant","participated") ->(V,Form(participant):SemanticState)) +
    (Seq("organizing","a organizer","organized") ->(V,Form(organizer):SemanticState)) +
    (Seq("what") ->(NP,identity)) +
    (Seq("me") ->(NP,identity)) +
    (Seq("what are all","by") ->(NP/NP,identity)) +
    (Seq("by") ->((NP\V)/NP,identity)) +
    (Seq("related") ->Seq(((NP\NP)\NP,identity),((NP\O),identity))) +
    (Seq("list") ->(NP,identity)) +
    (Seq("java") ->(O,Form(SearchString("Java")):SemanticState)) +
    (Seq("are") ->(NP\NP,identity)) +
    (Seq("and") ->((NP\NP)/NP,λ {d1:Date =>λ {d2:Date =>DateRange(d1,d2)}})) +
    (Seq("between") ->Seq(((NP\NP)/NP,λ { dateRange: DateRange  =>λ {eventType: EventType => listEvents(eventType,allRole,allEventCategory,None,Some(dateRange))}}),
                     /*TODO*/     ((NP\NP)/NP,λ { dateRange: DateRange => λ{i:Location =>λ { n3:EventType => listEvents(n3,allRole,allEventCategory,Some(i),Some(dateRange))}}}))) +
    (Seq("all","happening") ->Seq((NP\NP,identity),
                                  (NP/NP,identity))) +
    (Seq("the") ->((NP/NP)\NP,identity)) +
    (Seq("my") ->Seq(((NP/NP),λ { n3:EventType  => listEvents(n3)}),(NP/NP,identity))) +
    (Seq("in") ->Seq(((S/NP)\NP,λ { n3:EventType  =>λ {i:Location => listEvents(n3,allRole,allEventCategory,Some(i))}}),
                    /*((S\NP)/NP,λ { dateRange: DateRange => λ{i:Location =>λ { n3:EventType => listEvents(n3,allRole,allEventCategory,Some(i),Some(dateRange))}}}),*/
                    ((NP/NP),identity),
                    (((NP\O)/NP),identity),
                    (((NP/NP)\NP,identity)),
                    ((S/NP)\NP,λ { n3:EventType  =>λ {i:DateEntity => listEvents(n3,allRole,allEventCategory,None,Some(i))}})))

  // (("chennai") -> (NP,Form(Location("Chennai")):SemanticState))


  /*  Injecting lexicon definition for the entities identified from the sentence - Lexicons are intended
      to be injected in run time. */

  def injectLexicon[T <: Entity](entity:T):Unit = {
      val lexeme:(String,(ccg.NP.type,SemanticState)) = entity match {
        case d@Location(value:String) => ((value) -> (NP,Form(d):SemanticState))
        case d@DateString(value:String) => ((value) -> (NP,Form(d):SemanticState))
        case d@Date(value:String) => ((value) -> (NP,Form(d):SemanticState))
      }

    this.lexicon+=lexeme;
  }
}
