package it.flatmap.gieter

import sys.process._
import java.time._
import java.io.{File, FileOutputStream, PrintWriter}

import scala.util.{Failure, Success, Try}
import scala.io.Source

trait CommitDataGenerator {
  val width = 53
  val height = 7
  val painting: String =
                 """. . . . . . . . . . . . . . . . . . . . . * . . . * * . . . * * . . . . * * * * . . * . . * * * * * . . .
                   |. . . . . . . . . . . . . . . . . . . . . * . . * * * * . * * * * . . * . . . . . . * . . . . * . . . . .
                   |. . . . . . . . . . . . . . . . . . . . . * . . * * * * * * * * * . . * . . . . . . * . . . . * . . . . .
                   |. . . . . . . . . . . . . . . . . . . . . * . . . * * * * * * * . . . * . * * * . . * . . . . * . . . . .
                   |. . . . . . . . . . . . . . . . . . . . . * . . . . * * * * * . . . . * . . . * . . * . . . . * . . . . .
                   |. . . . . . . . . . . . . . . . . . . . . * . . . . . * * * . . . . . . * . . * . . * . . . . * . . . . .
                   |. . . . . . . . . . . . . . . . . . . . . * . . . . . . * . . . . . . . . * * * . . * . . . . * . . . . .""".stripMargin

  lazy val depthTemplates = Map('.' -> 0, '=' -> 1, '+' -> 2, '#' -> 3, '*' -> 4)

  def pixelDatesBuilder: Seq[(Char, LocalDate)] = {
    val dates = dateRange()
    val pixelDate = if(flattenedPainting.length >= dates.length)
      flattenedPainting.zip(dates)
    else
      flattenedPainting.zipAll(dates, '.', LocalDate.now())
    pixelDate
  }

  /* Date range iterator in steps of one date */
  private def dateRange() = {
    import java.time.temporal.ChronoUnit.DAYS
    val yearAgoSunday = getDateAYearAgoSunday
    val now = LocalDate.now

    val dates: Seq[LocalDate] = for {
      day <- 0 to DAYS.between(yearAgoSunday, now).toInt
    } yield yearAgoSunday.plusDays(day)
    dates
  }

  /* Gets the date a year ago last sunday
   */
  private def getDateAYearAgoSunday: LocalDate = {
    def getSunday(yearAgo: LocalDate): LocalDate = {
      yearAgo.getDayOfWeek match {
        case DayOfWeek.SUNDAY => yearAgo
        case _ => getSunday(yearAgo.plusDays(1L))
      }
    }

    getSunday(LocalDate.now().minusDays(width*height))
  }

  private def flattenedPainting: String = {
    val spaceCleanedPrinting = removePaintingSpaces

    val chars: Seq[Char] = for {
      i <- 0 until width
      j <- 0 until height
    } yield spaceCleanedPrinting(i + j * width)
    chars.mkString
  }

  private def removePaintingSpaces: String = painting.replaceAll("\\s","")

}

trait GitRepositoryBuilder {

  val tagname = "gieter-start"

  def createRepositoryFolder: File = {
    Process("pwd", new File(".")).!
    Process("rm -rf gieter_repo", new File(".")).!
    Process("mkdir gieter_repo", new File(".")).!

    val currentPath = new File(".").getAbsolutePath
    new File(currentPath, "gieter_repo")
  }

  /**
    * It initialize the git repo
    */
  def createRepo(repositoryFolder: File): Int = {

    Process("""git clone https://github.com/P3trur0/gieter-field.git""", repositoryFolder).!

    val repository = new File(repositoryFolder, "gieter-field")

    Process(s"""git config user.email "${System.getenv("GITHUB_EMAIL")}""", repository).!
    Process(s"""git config user.name "P3trur0"""", repository).!

    Process(s"""git reset --hard $tagname""", repository).!

    Process(s"""git tag -d $tagname""", repository).!

    val writer = new PrintWriter(new FileOutputStream(new File(repository, "README.md"),true))
    writer.append(s"----\nCommit time is **${ZonedDateTime.now()}**")
    writer.close()

    Process(s"""git add README.md""", repository).!
    Process(s"""git commit -m "GIeTer__started" """, repository).!
    Process(s"""git tag $tagname""", repository).!
  }

  def getMaxCommits: Try[Integer] = {
    Try {
      val regexGroups = """(.*)(data-count=")(\d*)(".*)""".r
      val lines = Source.fromURL("https://github.com/users/P3trur0/contributions", "UTF-8").getLines()
      val counts = lines.filter(_.contains("data-count")).map { line =>
        regexGroups findFirstIn line match {
          case Some(regexGroups(_, _, number, _)) => Integer.parseInt(number)
          case None => 0
        }
      }

      counts.max
    }
  }

  def getMeanNumberOfCommits(maxCommits: Integer): Try[Integer] = {
    Try {
      if(maxCommits<=0) 1
      else maxCommits/4
    }
  }

}

object Committer extends App with CommitDataGenerator with GitRepositoryBuilder {

  assert(painting.length!=width*height, s"Pattern must be $width by $height not ${painting.length}")

  private def commitIterations(repository: File, commitMultiplier: Integer) = {

    var totalCommits = 0

    val commitTemplates = depthTemplates.map {
      case(char, number) => (char, number*commitMultiplier)
    }

    pixelDatesBuilder.foreach {
      case (pixel: Char, date: LocalDate) => {
        var commitPerDay = 0
        (0 until commitTemplates(pixel)).foreach { _ =>
          Process(s"""git commit -m "pouring_water..." --allow-empty --date ${date.toString}T12:00:00""", new File(repositoryFolder, "gieter-field"))!

        commitPerDay = commitPerDay + 1
        }
        println(s"$commitPerDay commits on $date")
        totalCommits = totalCommits + commitPerDay
      }
    }

    println(s"Number of total commits = $totalCommits.")
  }

  def pushOnRemote(repository: File) = {
    val username = System.getenv("GIT_USERNAME")
    val password = System.getenv("GIT_PASSWORD")
    Process(Seq("git", "push", s"https://${username}:${password}@github.com/P3trur0/gieter-field.git", "-f"),
      new File(repositoryFolder, "gieter-field"))!ProcessLogger(_ => ())
  }

  val repositoryFolder: File = createRepositoryFolder
  val meanNumberOfCommits = getMaxCommits.flatMap(getMeanNumberOfCommits)

  meanNumberOfCommits match {
    case Success(x) => {
      createRepo(repositoryFolder)
      commitIterations(repositoryFolder, x)
      pushOnRemote(repositoryFolder)
    }
    case Failure(exception) => println(s"Failure while retrieving max number of commits: ${exception.getMessage}")
  }
}