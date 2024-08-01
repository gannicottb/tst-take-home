package com.github.gannicottb
import ConsoleUtils._

object Main {
  import SampleInput._

  def main(args: Array[String]): Unit =
    printAll(
      "PROBLEM 1",
      divider(),
      "OUTPUT",
      Solver.getBestGroupPrices(inputRates, inputPrices).sortBy(x => (x.cabinCode, x.rateCode)),
      divider(),
      divider("*"),
      divider(),
      "PROBLEM 2",
      divider(),
      "OUTPUT (ALL PROMOTIONS)",
      Solver.allCombinablePromotions(inputPromotions),
      "OUTPUT (P1)",
      Solver.combinablePromotions("P1", inputPromotions),
      "OUTPUT (P3)",
      Solver.combinablePromotions("P3", inputPromotions),
    )
}
