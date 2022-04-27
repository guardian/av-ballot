package com.gu.ballot.av.csv

import kantan.csv.{CellDecoder, DecodeResult}

object GoogleGroup {
  enum Role:
    case Member, Manager, Owner, Pending

  given CellDecoder[GoogleGroup.Role] = {
    val roleByName = Role.values.map(role => role.toString.toLowerCase -> role).toMap
    CellDecoder.from(s => DecodeResult(roleByName(s.toLowerCase)))
  }
}

