import com.typesafe.config.ConfigFactory
import movieFunctions.{getLanguageCounts, getLongestDurationTitle, getTopTitles, getTopTitlesByBudget, readCSV, searchByDirectorAndYearRange}
import scala.io.StdIn.readLine

object scalaAssessment {
  def main(args: Array[String]): Unit = {

    // reading variables from the config file
    val config = ConfigFactory.load("config.conf")
    val path = config.getString("file_path")
    val dataRows = readCSV(path)
    val director = config.getString("director")
    val startYear = config.getInt("startYear")
    val endYear = config.getInt("endYear")
    val languageFilter = config.getString("languageFilter")
    val userReviewFilter = config.getInt("userReviewFilter")
    val yearFilter = config.getString("yearFilter")
    val countryFilter = config.getString("countryFilter")
    val minNumVotes = config.getInt("minNumVotes")
    val budgetMin = config.getInt("budgetMin")
    val budgetMax = config.getInt("budgetMax")
    val budgetRange = budgetMin to budgetMax


    println("Enter an checkpoint:")
    println("1. Titles directed by given director in the given year range ")
    println("2. report of English titles which have user reviews more than given user review filter and sort the report with user reviews by descending  ")
    println("3. highest budget titles for the given year and country filters")
    println("4. report of longest duration title for the given country filter, no of minimum votes filter and sort by duration in descending order ")
    println("5. language wise report to count the titles for the given budget range and country filter and sort with count descending ")
    val input = readLine()

    // to choose desired option and print the output
    input match {
      case "1" =>
        searchByDirectorAndYearRange(director, startYear, endYear, dataRows)
      case "2" =>
        val topTitles = getTopTitles(dataRows, languageFilter, userReviewFilter)
        if (topTitles.nonEmpty) {
          println("Top titles in English with more than 10000 user reviews:")
          topTitles.foreach { case (title, numReviews) =>
            println(s"$title ($numReviews reviews)")
          }
        } else {
          println("No titles found matching the criteria.")
        }
      case "3" =>
        getTopTitlesByBudget(dataRows, countryFilter, yearFilter)
      case "4" =>
        getLongestDurationTitle(dataRows,countryFilter,minNumVotes )
      case "5" =>
        getLanguageCounts(dataRows,countryFilter,budgetRange)
      case _ =>
        println("Invalid input")
    }

  }

}
