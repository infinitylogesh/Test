/**
  * Created by prime on 21/3/17.
  */

package prime

import scala.io.Source._
import scala.annotation.meta.param
import java.util.Calendar
import java.io._
import com.workday.montague.semantics.Form

object regress {

  def renderResult() = {

    val lines = scala.io.Source.fromFile("src/main/resources/testQueries.txt").getLines();
    val testResults = for(line <- lines) yield(line,assertClass(line))
    val testResultsList = testResults.toList
    val summary = "Summary" + "\n" + "_______" + "\n" + testResultsList.groupBy(_._2).map(x=>(x._1,x._2.size)).toString + "\n\n" +"Result" + "\n" + "_______"
    val results = testResultsList.sortBy(_._2).map((x)=>(x._1+" --> "+x._2)).reduceLeft((x:String,y:String)=>(x+"\n"+y));

    println(summary)
    println(results)
    writeFile(summary+"\n"+results)
  }


  /*
  *  Asserts if the parsed value belongs to listEvents case class. If yes returns true else false.
  * */

  def assertClass(searchQuery:String):Boolean = {
    suTime.extractDates(searchQuery)
    val parserObj = new parser(LexiconOps.lexicon)
    val output = parserObj.customParser(parserObj.parse(searchQuery))
    val out = output.map(_.semantic);
    val result = out match {
      case Some(Form(listEvents(_,_,_,_,_,_,_))) => true // the semanticstate returned is always inside Form.
      case _ => false
    }
    result
  }

  def writeFile(content:String) = {
    new PrintWriter("results-log-"+Calendar.getInstance().getTimeInMillis+".txt") { write(content); close }
  }

}
