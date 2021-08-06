package sbnn

import cats.data.{Ior, NonEmptyList}
import io.circe
import io.circe.syntax.EncoderOps

import scala.util.Try

object Task {

  val supportedFilters: String Map UnsafeCheckBuilder[CompanyData] =
    Map(
      "IsProfitable" -> (_ => _.isProfitable),
      "NameContains" -> { params =>
        val text = params("text")
        _.companyName.contains(text)
      }
    )

  /**
   * true = passes the check
   */
  type Check[T]              = T => Boolean
  type UnsafeCheckBuilder[T] = Map[String, String] => Check[T]
  case class CompanyData(companyName: String, revenue: Int, isProfitable: Boolean)
  val filterType                         = "filterType"
  val supportedFiltersNames: Set[String] = supportedFilters.keySet
  sealed trait MyError { def isFatal = false }
  case class Warning(s: String) extends MyError
  case class Fatal(s: String)   extends MyError { override def isFatal = true }

  val parseCsv: String => Either[String, CompanyData] =
    s =>
      s.split(",").toList match {
        case name :: rev :: isProfitable :: Nil =>
          Try(CompanyData(name, rev.toInt, isProfitable.toBoolean)).toEither.left
            .map(e => s"Wrong type in string '$s': $e")
        case _ =>
          Left(s"Could not parse string '$s'")
      }

  private val makeRules =
    (_: NonEmptyList[circe.Error] Ior List[Map[String, String]])
      .leftMap(_.toList.map(e => Fatal(s"Could not read json: $e")))
      .flatMap { rules =>
        val (supported, notSupported) = rules.partition {
          _.get(filterType).exists(supportedFiltersNames)
        }
        val builderResults = supported.flatMap { jsonMap =>
          (for {
            filterName    <- jsonMap.get(filterType)
            unsafeBuilder <- supportedFilters.get(filterName)
          } yield filterName -> Try(unsafeBuilder(jsonMap))).toList
        }
        val builderErrors =
          builderResults.flatMap { nameAndTry =>
            nameAndTry._2.failed.toOption.toList
              .map(e => Fatal(s"Could not build rule '${nameAndTry._1}': $e"))
          }
        val unknownNamesErrors =
          notSupported
            .map(m =>
              Warning(s"Could not understand rule '${m.getOrElse(filterType, "")}': ${m.asJson.noSpacesSortKeys}")
            )
        val checks = builderResults.flatMap(_._2.toOption.toList)

        builderErrors ::: unknownNamesErrors match {
          case Nil  => Ior.Right(checks)
          case list => Ior.Both(list, checks)
        }
      }
      .map(checks => (d: CompanyData) => checks.forall(_(d)))

  private def exampleRun(csv: String, json: String): Unit = {
    import io.circe.parser._

    val rules          = makeRules(decodeAccumulating[List[Map[String, String]]](json).toIor)
    val parsingResults = csv.split("\n").toList.tail.map(parseCsv)
    val parsingErrors  = parsingResults.flatMap(_.left.toSeq)
    val data           = parsingResults.flatMap(_.toSeq)

    println(s"Errors while parsing data = $parsingErrors")
    println(s"Parsed data = $data")
    rules.left.foreach { errors =>
      println(s"There were${if (errors.exists(_.isFatal)) "" else " no"} fatal errors building the rules")
      errors.foreach(println)
    }
    rules.right.foreach { checks =>
      val filtered = data.filter(checks)
      println(s"Filtered data = $filtered")
      println(s"Top 3 by revenue in filtered = ${filtered.sortBy(_.revenue).reverse take 3}")
    }
  }

  def main(args: Array[String]): Unit = {

    // companies.scv
    val csv = """CompanyName,Revenue,IsProfitable
                |error string
                |"Company 0",100,1
                |"Company 1",100,true
                |"Company foo 1",101,true
                |"Company foo 2",102,false
                |"Company foo 3",103,true
                |"Company foo 4",104,true
                |"Company foo 5",105,true""".stripMargin.replace("\r", "")

    // filters.json
    val json = """[
                 |{
                 |"filterType": "NameContains",
                 |"text": "foo"
                 |},
                 |{
                 |"filterType": "NameContains",
                 |"otherParamName": "foo"
                 |},
                 |{
                 |"filterType": "IsProfitable"
                 |},
                 |{
                 |"filterName": "IsProfitable"
                 |}
                 |]""".stripMargin.replace("\r", "")

    exampleRun(csv, json)
  }

}
