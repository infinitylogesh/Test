package prime

import javax.swing.text.html.parser.Entity

import com.workday.montague.ccg.{CcgCat, NP, S}
import com.workday.montague.parser.{IntegerMatcher, ParserDict}
import com.workday.montague.semantics.{Form, SemanticState, identity, λ}
import com.workday.montague.ccg
/**
  * Created by prime on 16/3/17.
  */

object LexiconOps {

  implicit class pluralOps(val str:String) extends AnyVal{
    def s:Seq[String] = Seq(str,str+"s");
  }

  var lexicon = ParserDict[CcgCat]() +
    (("event".s) -> (NP,Form(allEventType):SemanticState)) +
    (("conference".s) -> (NP,Form(conference):SemanticState)) +
    (Seq("that") -> ((NP\NP),identity)) +
    (Seq("iam","i am") ->Seq(((S/V)\NP,λ { n3:EventType  => λ { n2:Role => listEvents(n3,n2)}}),
      (((S/NP)/V)\NP,λ { eventType:EventType  => λ { role :Role =>λ { entity:Entity=>listEvents(eventType,role,allEventCategory,Some(entity))}}}))) +
    (Seq("participating","participant","a participant") ->(V,Form(participant):SemanticState)) +
    (Seq("organizing","a organizer") ->(V,Form(organizer):SemanticState)) +
    (Seq("what") ->(Q,identity)) +
    (Seq("list") ->(NP,identity)) +
    (Seq("java") ->(NP/NP,Form(SearchString("Java")):SemanticState)) +
    (Seq("are") ->(NP\Q,identity)) +
    (Seq("all","happening") ->(NP\NP,identity)) +
    (Seq("the") ->((NP/NP)\NP,identity)) +
    (Seq("my") ->Seq((NP/NP,λ { n3:EventType  => listEvents(n3)}),(NP/NP,identity))) +
    (Seq("in") ->Seq(((S/NP)\NP,λ { n3:EventType  =>λ {i:Entity => listEvents(n3,allRole,allEventCategory,Some(i))}}),
      ((NP/NP,identity)),
      ((S/NP)\NP,λ { searchString: SearchString =>λ { n3:EventType  =>λ {i:Entity => listEvents(n3,allRole,allEventCategory,Some(i),Some(searchString))}}}) ))
   // (("chennai") -> (NP,Form(Location("Chennai")):SemanticState))


  /*  Injecting lexicon definition for the entities identified from the sentence - Lexicons are intended
      to be injected in run time. */

  def injectLexicon[T <: Entity](entity:T):Unit = {
      val lexeme:(String,(ccg.NP.type,SemanticState)) = entity match {
        case d@Location(value:String) => ((value) -> (NP,Form(d):SemanticState))
        case d@Date(value:String) => ((value) -> (NP,Form(d):SemanticState))
      }
    //lexicon += lexeme
    this.lexicon+=lexeme;
  }
}
