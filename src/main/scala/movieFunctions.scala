import java.io.FileNotFoundException
import scala.io.Source
import scala.util.Try

object movieFunctions {

// function for reading the CSV file
  def readCSV(path: String): Option[List[Map[String, String]]] = {
    try {
      val file = Source.fromFile(path)

      // read the header row
      val headerRow = file.getLines().next().split(",", -1)

      // read the data rows
      val dataRows = file.getLines().drop(1).take(10000).map { line =>
        // split each row into columns and handle cases where a column value contains commas
        val columns = line.split(",(?=([^\"]*\"[^\"]*\")*[^\"]*$)", -1).map(_.replaceAll("\"", ""))
        // create a map of column name to value
        headerRow.zip(columns).toMap
      }.toList

      file.close()

      // checking for file not found exception
      Some(dataRows)
    } catch {
      case e: FileNotFoundException => {
        println(s"Error: Could not find file at path: $path")
        None
      }
      case e: Exception => {
        println(s"Error: ${e.getMessage}")
        None
      }
    }
  }

// 1) function for finding the title for directors with a given year range
  def searchByDirectorAndYearRange(director: String, startYear: Int, endYear: Int, dataRows: Option[List[Map[String, String]]]): Unit = {

    dataRows match {
      case Some(rows) =>
        val matchingRows = rows.filter(row => {
         // getting rows director and year
          val directors = row.getOrElse("director", "")
          val yearString = row.getOrElse("year", "")
          // checking for the given director for particular year range
          if (directors.contains(director)) {
            if (yearString.nonEmpty) {
              val year = yearString.toInt
              year >= startYear && year <= endYear
            } else {
              false
            }
          } else {
            false
          }
        })

        // Print the matching rows as a report
        if (matchingRows.nonEmpty) {
          println("Titles directed by " + director + " in the year range " + startYear + " to " + endYear + ":")
          println("---------------------------------------------------------")
          matchingRows.foreach(row => {
            println(row("title") + " (" + row("year") + ")")
          })
        } else {
          println("No titles directed by " + director + " in the year range " + startYear + " to " + endYear + " were found.")
        }

      case None => println("Error: Data is missing.")
    }
  }

// 2)  getting top title for particular language and having reviews more than given reviews
  def getTopTitles(dataRows: Option[List[Map[String, String]]], languageFilter: String, userReviewFilter: Int): List[(String, Int)] = {
    dataRows match {
      case Some(rows) =>
        // Filter the data rows to include only the rows for the target language and user reviews
        val matchingRows = rows.filter(row => {
          val languageList = row.getOrElse("language", "").split(",").map(_.trim.toLowerCase)
          val numUserReviews = Try(row.getOrElse("reviews_from_users", "0").toInt).getOrElse(0)   // checking if the reviews columns has null or not,if it has empty values use 0
          languageList.contains(languageFilter.toLowerCase) && numUserReviews >= userReviewFilter
        })

        // Sort the matching rows by user reviews in descending order and take the top 10
        val topTitles = matchingRows.sortBy(row => row.getOrElse("reviews_from_users", "0").toInt)(Ordering[Int].reverse).take(10)

        // Return a List of tuples containing the title and number of user reviews
        topTitles.map(row => (row.getOrElse("title", ""), row.getOrElse("reviews_from_users", "0").toInt))
      case None =>
        // If dataRows is None, return an empty list
        List.empty[(String, Int)]
    }
  }



//  3  getting top titles for a particular year and country according to budget

  def getTopTitlesByBudget(dataRows: Option[List[Map[String, String]]], countryFilter: String, yearFilter: String): Unit = {
    dataRows match {
      case Some(rows) =>
        val matchingRows = rows.filter(row => {
          val countryList = row.getOrElse("country", "").split(",").map(_.trim.toLowerCase)
          val startYear = row.getOrElse("year", "")
          val budgetStr = row.getOrElse("budget", "").replaceAll("[^0-9]", "")
          val budget = Try(budgetStr.toDouble).getOrElse(0.0)                       // checking if the budget columns has null or not,if it has empty values use 0
          countryList.contains(countryFilter.toLowerCase) && startYear == yearFilter && budget > 0.0
        })

        // Sort the matching rows by budget in descending order and take the top 10
        val topTitles = matchingRows.sortBy(row => row.getOrElse("budget", "").replaceAll("[^0-9]", "").toDoubleOption.getOrElse(0.0))(Ordering[Double].reverse).take(10)

        // Print the top titles
        println("Top titles for " + countryFilter + " in " + yearFilter + " by budget:")
        topTitles.foreach(row => println(row.getOrElse("title", "") + " " + row.getOrElse("budget", "")))
      case None =>
        println("No data rows found.")
    }
  }


// 4  getting longest duration titles according to country and having min. votes

  def getLongestDurationTitle(dataRows: Option[List[Map[String, String]]], countryFilter: String, minNumVotes: Int): Unit = {
    dataRows match {
      case Some(rows) =>
        // filter the data by country and minimum number of votes, and sort by duration in descending order
        val matchingRows = rows.filter(row => {
          val countryList = row.getOrElse("country", "").split(",").map(_.trim.toLowerCase)
          val numVotes = Try(row.getOrElse("reviews_from_users", "0").toInt).getOrElse(0)    // checking if the reviews columns has null or not,if it has empty values use 0
          countryList.contains(countryFilter.toLowerCase) && numVotes >= minNumVotes
        }).sortBy(row => -1 * row.getOrElse("duration", "0").toInt)

        // print the report
        if (matchingRows.nonEmpty) {
          val longestDuration = matchingRows.head.getOrElse("duration", "0")
          println(s"The longest duration title for $countryFilter with at least $minNumVotes votes is $longestDuration minutes:")
          matchingRows.take(10).foreach(row => println(s"${row("title")} (${row("year")}) - ${row("duration")} minutes"))
        } else {
          println(s"No titles found for $countryFilter with at least $minNumVotes votes.")
        }
      case None =>
        println("Error: Data rows not found.")
    }
  }


// 5 function to get details according to language for a particular country within a given budget range

  def getLanguageCounts(dataRows: Option[List[Map[String, String]]], countryFilter: String, budgetRange: Range.Inclusive): Unit = {
    val filteredRows = dataRows.getOrElse(List.empty).filter(row => row("country") == countryFilter && {
      val budgetString = row("budget")
      budgetString.nonEmpty && budgetString(0) == '$' && {
        val budgetValue = Try(budgetString.replaceAll("[,$]", "").toInt).getOrElse(0)   // checking if the budget columns has null or not,if it has empty values use 0
        budgetRange.contains(budgetValue)
      }
    })

    val languageCounts = filteredRows.groupBy(_.getOrElse("language", "Unknown")).mapValues(_.size)

    val sortedCounts = languageCounts.toSeq.sortBy(-_._2)

    println(s"Language\tCount")
    for ((language, count) <- sortedCounts) {
      println(s"$language\t$count")
    }
  }


}
