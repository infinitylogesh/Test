package prime

import com.workday.montague.ccg.{CcgCat, NP, S}
import com.workday.montague.parser.ParserDict
import com.workday.montague.semantics.{Form, SemanticState, identity, λ}

/**
  * Created by prime on 16/3/17.
  */



object lexicon {

  implicit class pluralOps(val str:String) extends AnyVal{
    def s:Seq[String] = Seq(str,str+"s");
  }

  val lexicon = ParserDict[CcgCat]() +
    (("event".s) -> (NP,Form(allEventType):SemanticState)) +
    (("conference".s) -> (NP,Form(conference):SemanticState)) +
    (Seq("that") -> ((NP\NP),identity)) +
    (Seq("iam","i am") ->((S/V)\NP,λ { n3:EventType  => λ { n2:Role => listEvents(n3,n2)}})) +
    (Seq("participating","participant","a participant") ->(V,Form(participant):SemanticState)) +
    (Seq("organizing","a organizer") ->(V,Form(organizer):SemanticState)) +
    (Seq("what") ->(Q,identity)) +
    (Seq("are") ->(NP\Q,identity)) +
    (Seq("all") ->(NP\NP,identity)) +
    (Seq("the") ->((NP/NP)\NP,identity)) +
    (Seq("my") ->(NP/NP,λ { n3:EventType  => listEvents(n3)}))
}
