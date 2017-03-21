/**
  * Created by prime on 21/3/17.
  */

package prime

import io.Source._
import scala.annotation.meta.param
import com.workday.montague.semantics.Form

object regress {

  def renderResult() = {

    val lines = io.Source.fromFile("src/main/resources/testQueries.txt").getLines();
    val testResults = for(line <- lines) yield(line,assertClass(line))
    val testResultsList = testResults.toList
    println("Summary")
    println("_______")
    println("");
    println(testResultsList.groupBy(_._2).map(x=>(x._1,x._2.size)))
    println("");
    println("Result")
    println("_______")
    println("");
    testResultsList.sortBy(_._2).map((x)=>println(x._1+" --> "+x._2));
  }


  /*
  *  Asserts if the parsed value belongs to listEvents case class. If yes returns true else false.
  * */

  def assertClass(searchQuery:String):Boolean = {
    val output = parser.parse(searchQuery).bestParse
    val out = output.map(_.semantic);
    val result = out match {
      case Some(Form(listEvents(_,_,_,_,_,_))) => true // the semanticstate returned is always inside Form.
      case _ => false
    }
    result
  }

}
