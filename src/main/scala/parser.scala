package prime

import com.workday.montague.ccg._
import com.workday.montague.parser._
import com.workday.montague.semantics.{λ, _}

/**
  * Created by prime on 16/3/17.
  */
object parser extends SemanticParser[CcgCat](lexicon.lexicon){

  def parse(str: String): SemanticParseResult[CcgCat] = parse(str, tokenizer = parenTokenizer)

  // We need a custom tokenizer to separate parentheses from adjoining terms
  private def parenTokenizer(str: String) = {
    str.replace("(", " ( ").replace(")", " ) ").trim.toLowerCase.split("\\s+")
  }

}
