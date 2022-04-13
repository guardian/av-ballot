package com.gu.ballot

import com.ibm.icu.text.MessageFormat

extension [T](items: Seq[T])
  def zipWithOneBasedIndex: Seq[(T, Int)] =
    for ((item, index) <- items.zipWithIndex) yield (item, index + 1)

extension (n: Int)
  def ordinal = MessageFormat.format("{0,ordinal}", n)

case class OneBasedList[T](items: Seq[T]) {
  def numberedList(f: T => String): Seq[String] = for ((item, oneBasedIndex) <- items.zipWithOneBasedIndex) yield {
    s"$oneBasedIndex. ${f(item)}"
  }
}
