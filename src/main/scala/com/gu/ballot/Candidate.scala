package com.gu.ballot

case class Candidate(name:String) {
  override val toString: String = name
}

extension (candidates: Iterable[Candidate])

  def andJoin: String = candidates.toList.sortBy(_.name) match {
    case candidate :: Nil => candidate.name
    case candidate1 :: candidate2 :: Nil => s"$candidate1 and $candidate2"
    case candidate :: otherCandidates => s"$candidate, ${otherCandidates.andJoin}"
    case Nil => "no candidates"
  }

  def wasOrWere: String =
    s"${candidates.andJoin} ${if (candidates.size == 1) "was" else "were"}"