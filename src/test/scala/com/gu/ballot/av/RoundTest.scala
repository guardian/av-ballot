package com.gu.ballot.av

import cats.*
import cats.data.*
import cats.implicits.*
import com.gu.ballot.ExampleCandidates.*
import com.gu.ballot.VotesPerCandidate
import com.gu.ballot.VotesPerCandidate.VoteRank
import com.gu.ballot.VotesPerCandidate.VoteRank.{SingleCandidate, Tie}
import com.gu.ballot.av.Round.Outcome.*
import com.gu.ballot.av.Round.Outcome.Elimination.*
import kantan.csv.{ReadError, rfc}
import org.scalatest.Inside
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RoundTest extends AnyFlatSpec with Matchers with Inside {

  it should "acknowledge candidates even if those candidates did not come first in any preference" in {
    Round(
      Preference(A, B)
    ).candidates shouldBe Set(A,B)

    Round(
      Preference(A, B),
      Preference(C, D)
    ).candidates shouldBe Set(A,B,C,D)
  }

  it should "count first-preference votes for all candidates" in {
    Round(
      Preference(A, C),
      Preference(B, C)
    ).firstPreferenceVotes.rankedByVotes.toSeq shouldBe Seq(Tie(Set(A,B), 1), SingleCandidate(C, 0))
  }

  it should "count votes at different preference stages" in {
    Round(
      Preference(A, C, B),
      Preference(B, C, A),
      Preference(B, C, A)
    ).preferenceStages shouldBe Seq(
      VotesPerCandidate(Map(A -> 1, B -> 2, C -> 0)),
      VotesPerCandidate(Map(A -> 0, B -> 0, C -> 3)),
      VotesPerCandidate(Map(A -> 2, B -> 1, C -> 0))
    )
  }

  it should "remove a last-placed candidate, even if they got more votes at the next stage of preferences" in {
    val round = Round(
      Preference(A, C),
      Preference(B, C)
    )
    inside(round.outcome) {
      case elimination: Elimination =>
        elimination.approach.eliminatedCandidates shouldBe Set(C)
        elimination.nextRound.candidates shouldBe Set(A, B)
    }
  }

  it should "not crash if the result is a tie" in {
    val round = Round(Preference(A), Preference(B))
    round.firstPreferenceVotes.rankedByVotes.head.candidates shouldBe Set(A, B)
    round.outcome shouldBe EssentialTie(NonEmptySeq.one(Set(A, B)))
  }

  it should "ultimately respect Robert's Rules of Order (RRO) on IRV ties in last-place elimination" in {
    /**
     * Robert's Rules of Order say:
     *
     * "If at any point two or more candidates or propositions are tied for the least popular position,
     *  the ballots in their piles are redistributed in a single step, all of the tied names being treated
     *  as eliminated."
     *
     * In this implementation, we're choosing to interpret "tied for the least popular position" as specifically
     * being "tied for the least popular position after all 2nd, 3rd, etc preferences have been compared", though
     * when the rule was written it may have meant simply "tied for the least popular position amongst 1st preference
     * votes". One advantage of electronic voting is that it's easy to compare 2nd, 3rd, etc preferences at will.
     *
     * Regardless, RRO's rule about bulk-elimination of tied candidates provides a simple strategy for dealing
     * with last-place ties. It's quite blunt, and may gloss over situations where the truest reflection of
     * voter preferences would in fact be to declare an 'essential' tie.
     */
    val round1 = Round(
      Preference(A, B), Preference(A, C),
      Preference(B, A), Preference(B, A),
      Preference(C, A), Preference(C, A)
    )
    inside(round1.outcome) {
      case elimination: Elimination =>
        inside(elimination.approach) {
          case path: TieResolution => path.eliminatedCandidates shouldBe Set(B, C)
        }
        elimination.nextRound.outcome shouldBe ClearWinner(A)
    }
    round1.ultimateConclusion shouldBe ClearWinner(A)
  }


  it should "perform bulk elimination to avoid irrelevant ties" in {
    /* From https://en.wikipedia.org/wiki/Instant-runoff_voting#Process :
     * "If there is an exact tie for last place in numbers of votes, various tie-breaking rules
     *  determine which candidate to eliminate. The set of all candidates with the fewest first-order
     *  votes whose votes together total less than any other candidate's can be eliminated without
     *  changing the outcome. This bulk elimination can bypass irrelevant ties, for example if one
     *  candidate receives 15 first-order votes and four others receive 5, 5, 3, and 1, and no other
     *  candidate receives fewer than 15, all four of the latter candidates will be eliminated during
     *  the next four rounds, and so can be eliminated immediately without considering the tie."
     */
    val round1 = Round(
      Preference(A) -> 16,
      Preference(B) -> 15,
      Preference(C) -> 5, Preference(D) -> 5,
      Preference(E) -> 3,
      Preference(F) -> 1
    )
    println(round1.summary)

    inside(round1.outcome) {
      case elimination: Elimination => inside(elimination.approach) {
        case path: FewerFirstPreferenceVotes => path.eliminatedCandidates shouldBe Set(C, D, E, F)
      }
    }
  }

}
