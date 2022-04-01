package com.gu.ballot

type FrequencyMap[T] = Map[T, Int]

extension [T](items: Iterable[T])
  def frequencyCount: FrequencyMap[T] = items.groupMapReduce(identity)(_ => 1)(_ + _)
extension [T](items: Iterable[(T, Int)])
  def sumFrequencies: FrequencyMap[T] = items.groupMapReduce(_._1)(_._2)(_ + _)
