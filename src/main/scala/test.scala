package prime

import scala.io.Source.stdin

/**
  * Created by prime on 16/3/17.
  */
object test {

  def main(args: Array[String]): Unit = {
    print(">> ")
    for(line <- stdin.getLines()){
      val output = parser.parse(line).bestParse;
      println(output);
      print(">> ");
    }
  }

}
