package utils

import java.io._
import scala.io.Source
import scala.reflect.ClassTag

/**
 * Please put your description here!
 *
 * @author sinhns2
 * @since 2024/1/5
 */
object CommonUtils {
  def fetchInput(day: Int, year: Int, sessionCookie: String = "53616c7465645f5f5f53d22c3530bf936d096bb2016e44aa764d138320407dfe5fe9551226ec7567c154807e8274581695f2e647d162371eb3e86466e4a91f41"): Unit = {
    val url = s"https://adventofcode.com/$year/day/$day/input"
    val headers = Map("Cookie" -> s"session=$sessionCookie")

    val response = requests.get(url, headers = headers)

    if (response.statusCode == 200) {

      if (!File(s"src/main/scala/data/$day.txt").exists){
        val content = response.text()
        val file = new File(s"src/main/scala/data/$day.txt")
        val writer = new PrintWriter(file)
        writer.write(content)
        writer.close()
        println(s"Fetch input to /src/main/scala/data/$day.txt successful")
      }
      else {
        println("Input already fetched")
      }

    } else {
      throw new RuntimeException(s"Failed to fetch input. Status code: ${response.statusCode}")
    }
  }

  def convert2arr[R: ClassTag](day: Int, split_by: Char = ' '): Array[Array[R]] = {
    val source = Source.fromFile(s"src/main/scala/data/$day.txt")
    val lines = try source.getLines().toList finally source.close()

    var result = Array[Array[R]]()
    for (line <- lines) {
      val convertedArray = line.toArray.map { str =>
        val convertedValue: R = implicitly[ClassTag[R]] match {
          case t if t.runtimeClass == classOf[Int]   => str.toInt.asInstanceOf[R]
          case t if t.runtimeClass == classOf[Char]  => str.asInstanceOf[R]
          case t if t.runtimeClass == classOf[Long]  => str.toLong.asInstanceOf[R]
          case _                                     => throw new IllegalArgumentException("Unsupported type")
        }
        convertedValue
      }
      result = result :+ convertedArray
    }
    result
  }
}
