package prime

import com.workday.montague.ccg.TerminalCat

/**
  * Created by prime on 16/3/17.
  */

case object Q extends TerminalCat { val category = "Q" }
case object V extends TerminalCat { val category = "V" } /* Category to represent all the verbs */
case object A extends TerminalCat { val category = "A" } /* to include Apps like d&i,hub etc */
case object O extends TerminalCat { val category = "O" } /* objects like JAVA etc*/
case object I extends TerminalCat { val category = "I" } /* internal commands */

