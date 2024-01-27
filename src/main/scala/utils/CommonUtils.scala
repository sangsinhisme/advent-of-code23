package utils

import java.io.{File, PrintWriter}
import scala.io.Source
import scala.reflect.ClassTag
import org.jsoup.Jsoup
import com.typesafe.config.{Config, ConfigFactory}

/**
 * Please put your description here!
 *
 * @author sinhns2
 * @since 2024/1/5
 */

object CommonUtils {
  def fetchInput(day: Int, year: Int): Unit = {
    val url = s"https://adventofcode.com/$year/day/$day/input"
    val session = ConfigFactory.load().getString("session")

    val headers = Map("Cookie" -> s"session=$session")

    val response = requests.get(url, headers = headers)

    if (response.statusCode == 200) {

      if (!File(s"src/main/resources/data/$day.txt").exists){
        val content = response.text()
        val file = new File(s"src/main/resources/data/$day.txt")
        val writer = new PrintWriter(file)
        writer.write(content)
        writer.close()
        println(s"Fetch input to /src/main/resources/data/$day.txt successful")
      }
      else {
        println("Input already fetched")
      }

    } else {
      throw new RuntimeException(s"Failed to fetch input. Status code: ${response.statusCode}")
    }
  }

  def convert2string(day: Int): Array[String] = {
    val source = Source.fromFile(s"src/main/resources/data/$day.txt")
    val lines = try source.getLines().toList finally  source.close()
    lines.toArray
  }

  def convert2list(day: Int): List[String] = {
    val source = Source.fromFile(s"src/main/resources/data/$day.txt")
    val lines = try source.getLines().toList finally  source.close()
    lines
  }

  def convert2string(file: String): Array[String] = {
    val source = Source.fromFile(s"$file")
    val lines = try source.getLines().toList finally  source.close()
    lines.toArray
  }

  def convert2list(file: String): List[String] = {
    val source = Source.fromFile(s"$file")
    val lines = try source.getLines().toList finally  source.close()
    lines
  }

  def convert2arr[R: ClassTag](day: Int): Array[Array[R]] = {
    val source = Source.fromFile(s"src/main/resources/data/$day.txt")
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

  def convert2arr[R: ClassTag](file: String): Array[Array[R]] = {
    val source = Source.fromFile(s"$file")
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

  def extractArticle(html: String): String = {
    val doc = Jsoup.parse(html)
    val articleContent = doc.select("article").text()
    articleContent
  }

  def submit_answer(day: Int, year: Int, answer: String, level: Int): Unit = {
    val url = s"https://adventofcode.com/$year/day/$day/answer"
    val session = ConfigFactory.load().getString("session")

    val headers = Seq(
      "Cookie" -> s"session=$session",
      "Content-Type" -> "application/x-www-form-urlencoded"
    )

    val postData = requests.post(
      url = url,
      headers = headers,
      data = s"level=$level&answer=$answer"
    )
    val articleContent = extractArticle(postData.data.toString)
    if articleContent.contains("That's not the right answer") then
      println(Console.RED + articleContent)
    else
      println(Console.GREEN + articleContent)
  }

  def transpose(input: List[String]): List[String] = input.map(_.toList).transpose.map(_.mkString)
}