package prime

import com.workday.montague.ccg.TerminalCat

/**
  * Created by prime on 16/3/17.
  */

case object Q extends TerminalCat { val category = "Q" }
case object E extends TerminalCat { val category = "E" } /* Events - Custom CCG category */
case object V extends TerminalCat { val category = "V" }