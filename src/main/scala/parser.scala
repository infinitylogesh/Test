package prime

import com.workday.montague.ccg._
import com.workday.montague.parser._
import com.workday.montague.cky.Chart
import com.workday.montague.semantics.{λ, _}

/**
  * Created by prime on 16/3/17.
  */
object parser extends SemanticParser[CcgCat](LexiconOps.lexicon){

  val stopWords = List("that","related","to");

  def parse(str: String): SemanticParseResult[CcgCat] = parse(str, tokenizer = parenTokenizer)

  // We need a custom tokenizer to separate parentheses from adjoining terms
  private def parenTokenizer(str: String) = {
    val splitString = str.replace("(", " ( ").replace(")", " ) ").trim.toLowerCase.split("\\s+")
    splitString.filterNot(stopWords.contains(_));
  }

  /*
  *  custom parser to replace the built in bestParse in montague . bestParse is replaced coz it just takes the first value
  *  as the result. This resulted in outputing Partial function for a parsable syntax with two semantics. the first syntax was taken.
  *  If the first syntax had partial function , Lambda function is given as out put
  *
  *  ex : 1. (((NP\O)/NP),λ {dateEntity: DateEntity =>λ {searchString: SearchString  =>  listEvents(allEventType,allRole,allEventCategory,None,Some(dateEntity),Some(searchString))}}), // java events happening this week
  *       2. ((NP/NP),λ { date:DateEntity  =>  listEvents(allEventType,allRole,allEventCategory,None,Some(date))}) // events this week

         events this week --> produced lambda function as it took the first lexicon instead of second.

      The custom parser looks for Statement case classes in the parsed list and it is outputed.
  * */

  def customParser(result:SemanticParseResult[CcgCat]):Option[SemanticParseNode[CcgCat]] = {
        val parsedList = result.chart(0,result.chart.n -1)
        parsedList.filter(x=>{
          x.semantic match {
            case Form(listEvents) => true
            case _ => false
          }
        }).headOption
  }

}
