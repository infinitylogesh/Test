/**
  * Created by prime on 29/3/17.
  */
package prime

import java.util.Calendar

import scala.util.matching.Regex

object dateOps {
  val weekRegex:Regex = "([0-9-]+)-([0-9]+)-W([0-9]+)".r  //2017-03-W13
  val monthRegex:Regex = "([0-9-]{4})-([0-9]{2})".r  //2017-04
  val yearRegex:Regex = "([0-9-]{4})".r //2018
  val dateRegex:Regex = "([0-9-]{4})-([0-9]{2})-([0-9]{2})".r //2017-01-20
  val calendar:Calendar = {     // Calendar is initiated and reset to 00:00:00:00
  val cl = Calendar.getInstance()
    cl.set(Calendar.HOUR,0)
    cl.set(Calendar.HOUR_OF_DAY,0)
    cl.set(Calendar.MINUTE,0)
    cl.set(Calendar.SECOND,0)
    cl.set(Calendar.MILLISECOND,0)
    cl
  }

  /* Regex match is done and the values are passed to required functions to get the date range
   *  Returns -> Tuple2 of time in milliseconds - Epoch time
   *  @param date : Temporal values as string from Stanford NLP
    * */

  def getDateRange(date:String):Option[(Long,Long)] = {
    date match {
      case weekRegex(year,month,weekOfYear) => processWeek(year.toInt,month.toInt,weekOfYear.toInt)
      case monthRegex(year,month) => processMonth(year.toInt,month.toInt)
      case yearRegex(year) => processYear(year.toInt)
      case dateRegex(year,month,day) =>  processDate(year.toInt,month.toInt,day.toInt)
      case _ => None
    }
  }

  /*
  *  Returns tuple of start of week and end of week time in milliseconds.
  * */
  def processWeek(year:Int,month:Int,weekOfYear:Int):Option[(Long,Long)] = {
    calendar.set(Calendar.YEAR,year)
    calendar.set(Calendar.MONTH,(month-1)) // Month starts from 0 in Java.util.Calendar
    calendar.set(Calendar.DAY_OF_WEEK,calendar.getFirstDayOfWeek) // Setting the day to the first day of the week
    calendar.set(Calendar.WEEK_OF_YEAR,weekOfYear)
    val currentDate =  calendar.getTimeInMillis
    println(calendar.getTime)
    calendar.add(Calendar.DAY_OF_MONTH,7) // Adding 7 days to get the EOW date.
    val nextDate = calendar.getTimeInMillis
    println(calendar.getTime)
    Some((currentDate,nextDate))
  }
  /*
   *  Returns tuple of start of month and end of year month in milliseconds.
   * */
  def processMonth(year:Int,month:Int):Option[(Long,Long)] = {
    calendar.set(Calendar.YEAR,year)
    calendar.set(Calendar.MONTH,(month-1)) // Month starts from 0 in Java.util.Calendar
    calendar.set(Calendar.DAY_OF_MONTH,1) // 1st day of the month.
    val currentDate = calendar.getTimeInMillis
    println(calendar.getTime)
    calendar.set(Calendar.DAY_OF_MONTH,calendar.getActualMaximum(Calendar.DAY_OF_MONTH)) // No. of days in a Month are added to get the last day of month
    val nextDate = calendar.getTimeInMillis
    println(calendar.getTime)
    Some((currentDate,nextDate))
  }

  /*
 *  Returns tuple of start of year and end of year time in milliseconds.
 * */
  def processYear(year:Int):Option[(Long,Long)] = {
    calendar.set(Calendar.YEAR,year)
    calendar.set(Calendar.MONTH,0) // month is set to January - 0
    calendar.set(Calendar.DAY_OF_MONTH,1) // 1st day of the month.
    val currentDate = calendar.getTimeInMillis
    println(calendar.getTime)
    calendar.set(Calendar.MONTH,12) // month is set to Jan of next year - 0
    val nextDate = calendar.getTimeInMillis
    println(calendar.getTime)
    Some((currentDate,nextDate))
  }

  /*
 *  Returns tuple of a day in milliseconds.
 * */
  def processDate(year:Int,month:Int,day:Int):Option[(Long,Long)] = {
    calendar.set(Calendar.YEAR,year)
    calendar.set(Calendar.MONTH,(month-1))
    calendar.set(Calendar.DAY_OF_MONTH,day)
    val currentDate =  calendar.getTimeInMillis
    println(calendar.getTime)
    Some((currentDate,currentDate))
  }



}

