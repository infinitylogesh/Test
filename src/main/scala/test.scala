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
      LexiconOps.injectLexicon(Date("this week"))
      //((chn.value) -> (NP,Form(chn):SemanticState))
      //val output = preProcess.parseSentence(preProcess.modelLocation,line);
      val output = parser.parse(line).bestParse
      println(output)
      print(">> ")
    }
  }

}
