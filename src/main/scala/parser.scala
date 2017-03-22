package prime

import com.workday.montague.ccg._
import com.workday.montague.parser._
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

}
