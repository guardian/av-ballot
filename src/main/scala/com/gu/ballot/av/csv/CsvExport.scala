package com.gu.ballot.av.csv

import com.gu.ballot.av.BallotCount
import kantan.csv.*
import kantan.csv.ops.*
import math.Ordering.Implicits.*

import java.io.File.createTempFile

object CsvExport {
  def exportAnonymisedData(ballotCount: BallotCount): Unit = {
    val out = createTempFile("anonymised", ".csv")
    println(out)

    val writer = out.asCsvWriter[Seq[String]](
      rfc.withHeader("Count" +: (1 to ballotCount.numCandidates).map(index => s"Preference $index") *)
    )
    for {
      (preference, count) <- ballotCount.countByPreference.toSeq.sortBy(_._1.order.map(_.name).toSeq)
    } {
      writer.write(count.toString +:preference.order.map(_.name).toSeq)
    }

    // Makes sure resources are freed.
    writer.close()
  }
}
