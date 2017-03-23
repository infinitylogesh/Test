package prime

import com.workday.montague.ccg.CcgCat
import com.workday.montague.parser.TokenMatcher

/**
  * Created by prime on 23/3/17.
  */

/*
*  Matches the apps in events and produces Lexicons.
* */

case class attributeMatcher(attribute:String) extends TokenMatcher[String]{

  def apply(str : String):Seq[String] = {
    attributes.attributesMap(attribute).find(x=>{x==str}) match {
      case Some(attr) => Seq(attr)
      case _ => Nil
    }
  }

}



