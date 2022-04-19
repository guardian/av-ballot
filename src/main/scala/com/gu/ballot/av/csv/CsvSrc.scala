package com.gu.ballot.av.csv

import kantan.csv.*
import kantan.csv.ops.*
import java.io.File

sealed trait CsvSrc[T] {
  def inputSrc(): CsvReader[ReadResult[T]]
}

object CsvSrc {
  case class FileSrc[B: HeaderDecoder](file: File) extends CsvSrc[B] {
    override def inputSrc(): CsvReader[ReadResult[B]] = file.asCsvReader[B](rfc)
  }

  case class UrlSrc[B: HeaderDecoder](url: java.net.URL) extends CsvSrc[B] {
    override def inputSrc(): CsvReader[ReadResult[B]] = url.asCsvReader[B](rfc)
  }
}