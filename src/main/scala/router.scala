package prime

import com.workday.montague.ccg.CcgCat
import com.workday.montague.parser.SemanticParseNode
import com.workday.montague.semantics.Form
import com.workday.montague.semantics.SemanticState

/**
  * Created by prime on 21/3/17.
  */
object router {

  /*
  *  routes to the required functions based on the Semantic state.
  * */
  def route(output:Option[SemanticParseNode[CcgCat]]) = {
    val semanticState = output.map(_.semantic)
    semanticState match {
      case Some(Form(run(x))) => x match {  // in case of run case class -> routes to run the internal command.
        case regression:regression.type => regress.renderResult();
      }
      case _ => println(output); // TODO : Remove after usage
    }
  }

}
