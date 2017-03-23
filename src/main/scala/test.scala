package prime

import scala.io.Source.stdin
import com.workday.montague.ccg.{CcgCat, NP, S}
import com.workday.montague.semantics.{Form, SemanticState, identity, λ}


/**
  * Created by prime on 16/3/17.
  */
object test {

  def main(args: Array[String]): Unit = {
    print(">> ")
    for(line <- stdin.getLines()){

      val chn = Location("chennai")
      LexiconOps.injectLexicon(Location("chennai"))
      LexiconOps.injectLexicon(Location("bangalore"))
      LexiconOps.injectLexicon(Location("mumbai"))
      LexiconOps.injectLexicon(DateString("this week"))
      LexiconOps.injectLexicon(DateString("this month"))
      LexiconOps.injectLexicon(DateString("today"))
      LexiconOps.injectLexicon(Date("02/03/1990"))
      LexiconOps.injectLexicon(Date("1st"))
      LexiconOps.injectLexicon(Date("2nd"))
      LexiconOps.injectLexicon(SearchString("oracle"))

      //((chn.value) -> (NP,Form(chn):SemanticState))
      //val output = preProcess.parseSentence(preProcess.modelLocation,line);
      val parsedLine = parser.parse(line);
      val output = parser.customParser(parsedLine)
   //   parser.parse(line).bestParse
      router.route(output)
      print(">> ")
    }
  }

}
