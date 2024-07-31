package com.github.gannicottb
import Model._

object Main {

  val sampleRates = List(
    Rate("M1", "Military"),
    Rate("M2", "Military"),
    Rate("S1", "Senior"),
    Rate("S2", "Senior")
  )

  val samplePrices = List(
    CabinPrice("CA", "M1", 200.00),
    CabinPrice("CA", "M2", 250.00),
    CabinPrice("CA", "S1", 225.00),
    CabinPrice("CA", "S2", 260.00),
    CabinPrice("CB", "M1", 230.00),
    CabinPrice("CB", "M2", 260.00),
    CabinPrice("CB", "S1", 245.00),
    CabinPrice("CB", "S2", 270.00)
  )

  val expected = List(
    BestGroupPrice("CA", "M1", 200.00, "Military"),
    BestGroupPrice("CA", "S1", 225.00, "Senior"),
    BestGroupPrice("CB", "M1", 230.00, "Military"),
    BestGroupPrice("CB", "S1", 245.00, "Senior")
  )

  val samplePromotions =  List(
    Promotion("P1", Seq("P3")),
    Promotion("P2", Seq("P4", "P5")),
    Promotion("P3", Seq("P1")),
    Promotion("P4", Seq("P2")),
    Promotion("P5", Seq("P2")),
  )
  val expectedAll = Set(
    PromotionCombo(Seq("P1", "P4", "P5")),
    PromotionCombo(Seq("P1", "P2")),
    PromotionCombo(Seq("P2", "P3")),
    PromotionCombo(Seq("P3", "P4", "P5"))
  )

  def main(args: Array[String]) = {
    println(sampleRates)
    println(samplePrices)
    println("-" * 10)
    val bestPrices = Solver.getBestGroupPrices(sampleRates, samplePrices)
    println(bestPrices.sortBy(x => (x.cabinCode, x.rateCode)))
    println(expected)
    if(bestPrices.toSet == expected.toSet) println("PASS")
    else println(s"FAIL: did not match ${expected.toSet}")
    println("--promos--")
    println(samplePromotions)
    val allCombos = Solver.allCombinablePromotions(samplePromotions)
    val combosForP1 = Solver.combinablePromotions("P1", samplePromotions)
    val combosForP3 = Solver.combinablePromotions("P3", samplePromotions)
    println(allCombos)
    println(combosForP1)
    println(combosForP3)
    if (allCombos.toSet == expectedAll) println("PASS")
    else println(s"FAIL: did not match ${expectedAll}")

  }
}
