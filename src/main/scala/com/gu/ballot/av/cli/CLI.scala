package com.gu.ballot.av.cli

import kantan.csv.*
import kantan.csv.ops.*
import com.gu.ballot.av.BallotReport
import com.gu.ballot.av.csv.CsvImport.*
import com.gu.ballot.av.csv.CsvSrc.FileSrc
import com.gu.ballot.av.csv.{CsvImport, ElectorateCsvParser, VotesCsvParser}
import scopt.*

import java.io.File

case class CLIConfig(votesFile: File, electorateFile: Option[File] = None)

object CLIConfig {

  val builder: OParserBuilder[CLIConfig] = OParser.builder[CLIConfig]
  val parser1 = {
    import builder.*
    OParser.sequence(
      opt[File]("votes")
        .required()
        .valueName("<file>")
        .action((file, c) => c.copy(votesFile = file))
        .text("CSV file containing the votes data"),
      opt[File]("electorate")
        .optional()
        .valueName("<file>")
        .action((file, c) => c.copy(electorateFile = Some(file)))
        .text("CSV file containing the electorate data, just 1 column with valid email addresses")
    )
  }
}

@main def main(args: String*) = {
  OParser.parse(CLIConfig.parser1, args, CLIConfig(new File("."))) match {
    case Some(config) =>
      importData(FileSrc(config.votesFile), config.electorateFile.map(FileSrc.apply))
    case _ =>
    // arguments are bad, error message will have been displayed
  }

}
