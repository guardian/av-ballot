package com.gu.ballot

case class Candidate(name:String)

extension (candidates: Iterable[Candidate])

  def wasOrWere: String =
    s"${candidates.toSeq.sortBy(_.name).mkString(", ")} ${if (candidates.size == 1) "was" else "were"}"