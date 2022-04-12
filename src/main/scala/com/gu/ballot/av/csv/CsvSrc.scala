package com.gu.ballot.av.csv

import kantan.csv.*
import kantan.csv.ops.*
import java.io.File

sealed trait CsvSrc {
  def inputSrc(): CsvReader[ReadResult[List[String]]]
}

object CsvSrc {
  case class FileSrc(file: File) extends CsvSrc {
    override def inputSrc(): CsvReader[ReadResult[List[String]]] = file.asCsvReader[List[String]](rfc)
  }

  case class UrlSrc(url: java.net.URL) extends CsvSrc {
    override def inputSrc(): CsvReader[ReadResult[List[String]]] = url.asCsvReader[List[String]](rfc)
  }
}