import java.time.format.DateTimeFormatter
import java.time.LocalDateTime
import java.time.LocalDate
import scala.io.Source

case class Data(dateTime: LocalDateTime, tempAvg: Double)
object MyProgram1 extends App {
  val pattern1 = "yyyyMMdd'T'HHmm"

  val pattern2 = "yyyy-MM-dd"
  val dateFormater1 = DateTimeFormatter.ofPattern(pattern1)

  val dateFormater2 = DateTimeFormatter.ofPattern(pattern2)


  val dataSeq = Source.fromFile("dataexport_20201105T020952.csv").getLines()
    .map(dataRow => Data(LocalDateTime.parse(dataRow.toString.split(",")(0),dateFormater1),dataRow.toString.split(",")(1).toDouble)).toSeq
    .sortWith((x,y) => x.dateTime.isBefore(y.dateTime))

  val requiredDataSeq = dataSeq.filter(y => y.dateTime.isBefore(LocalDateTime.parse("20201105T0000", dateFormater1)) &&
    y.dateTime.isAfter(LocalDateTime.parse("20201022T0000", dateFormater1).minusDays(1)))


  val requiredDateSet = requiredDataSeq.map(_.dateTime).map(_.toLocalDate).toSet

  // calculating the average temperature of each day
  val result1 = for {
    date <- requiredDateSet
  } yield {
    val averageTemp = (requiredDataSeq.filter(_.dateTime.toLocalDate.equals(date)).map(_.tempAvg).sum)/(requiredDataSeq.filter(_.dateTime.toLocalDate.equals(date)).size)
    Data(date.atStartOfDay(),averageTemp)
  }

  // displaying the average temperature of each day
  result1.toSeq
    .sortWith((x,y) => x.dateTime.isBefore(y.dateTime))
    .foreach(f => println(s"the average temperature for ${f.dateTime.format(DateTimeFormatter.ofPattern("yyyy-MM-dd"))} is ${f.tempAvg}"))


  // displaying the average temperature of each 7-day-interval
  val result2 = result1.toSeq
    .sortWith((x,y) => x.dateTime.isBefore(y.dateTime)).sliding(7,7).toSeq
  println("")
  println("the average temperature for each 7 day interval:")
  result2.foreach( interval =>
    println(interval.map(_.tempAvg).sum/interval.size)
  )

}
