package com.github.gannicottb

object ConsoleUtils {
  def printAll(i: Any*) =
    i.map {
      case s: Seq[_] => s.mkString("\n")
      case other => other
    }.foreach(println)

  def divider(s: String = "-") = s * 10
}
