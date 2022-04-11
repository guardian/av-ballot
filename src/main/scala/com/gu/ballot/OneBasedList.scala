package com.gu.ballot

case class OneBasedList[T](items: Seq[T]) {
  def numberedList(f: T => String): Seq[String] = for ((item, index) <- items.zipWithIndex) yield {
    s"${index + 1}. ${f(item)}"
  }
}
