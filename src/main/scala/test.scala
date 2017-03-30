package prime

import scala.io.Source.stdin
import com.workday.montague.ccg.{CcgCat, NP, S}
import com.workday.montague.semantics.{Form, SemanticState, identity, λ}
import io.circe._, io.circe.parser._
import io.circe.syntax._
import io.circe.generic.auto._
import io.circe.generic.JsonCodec

// TODO : Fix : Java related events -> Should have atleast one filter data

/**
  * Created by prime on 16/3/17.
  */
object test {

  val http = HTTPClient()
  if(suTime.isInitialized) println("Log: SuTime Initialized")

  def main(args: Array[String]): Unit = {
    print(">> ")
    for(line <- stdin.getLines()){
      val chn = Location("chennai")
      LexiconOps.injectLexicon(Location("chennai"))
      LexiconOps.injectLexicon(Location("bangalore"))
      LexiconOps.injectLexicon(Location("mumbai"))
      LexiconOps.injectLexicon(Location("pune"))
      LexiconOps.injectLexicon(Location("ahmedabad"))
     /* LexiconOps.injectLexicon(DateString("this month"))
      LexiconOps.injectLexicon(DateString("today"))
     // LexiconOps.injectLexicon(Date("02/03/1990"))*/
      //LexiconOps.injectLexicon(DateString("1st of april"))
      //LexiconOps.injectLexicon(DateString("2nd of april"))
      LexiconOps.injectLexicon(SearchString("oracle"))
      LexiconOps.injectLexicon(SearchString("aws"))
      LexiconOps.injectLexicon(SearchString("j2ee"))
      LexiconOps.injectLexicon(SearchString("ux"))

     // val sampleJson = jsonOps.primeEvent.asJson.noSpaces.replace("\"Type\":","\"type\":")


      //((chn.value) -> (NP,Form(chn):SemanticState))
   val output = preProcess.parseSentence(preProcess.modelLocation,line);
      println(output)
     suTime.extractDates(line)
      val parsedLine = parser.parse(line)
      val ParsedOutput = parser.customParser(parsedLine)
   //   parser.parse(line).bestParse
      router.route(ParsedOutput)
      print(">> ")
    }
  }

}
