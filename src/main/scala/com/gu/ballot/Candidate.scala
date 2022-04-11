package com.gu.ballot

case class Candidate(name:String) {
  override val toString: String = name
}

extension (candidates: Iterable[Candidate])

  def wasOrWere: String =
    s"${candidates.toSeq.sortBy(_.name).mkString(", ")} ${if (candidates.size == 1) "was" else "were"}"