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
      lexicon.lexicon = (lexicon.lexicon + (("chennai")->(E,Form("Chennai"):SemanticState)))
      val output = parser.parse(line).bestParse;
      println(output);
      print(">> ");
    }
  }

}
