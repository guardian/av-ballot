import kantan.csv.rfc
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import kantan.csv._
import kantan.csv.ops._
import cats.implicits._

class CsvTest extends AnyFlatSpec with Matchers {
  it should "be a temporary test that prints stuff " in {
    val rawData: java.net.URL = getClass.getResource("/dummy-election.responses.csv")

    val reader = rawData.asCsvReader[List[String]](rfc)

    val header: List[String] = reader.next().toOption.get
    val responses: (Seq[ReadError], Seq[List[String]]) = reader.toSeq.separate
    println(header)
    val voteRows: Seq[List[String]] = responses._2
    println(voteRows)
    val indiciesOfVoteColumns = header.indices.drop(1)
    val allSeenCandidates = voteRows.toSet.flatMap[String] { voteRowValues =>
      val preference: Seq[String] = indiciesOfVoteColumns.flatMap[String](voteRowValues.get(_))
      preference.toSet
    }


    println(allSeenCandidates.toSeq.sorted)
  }
}
