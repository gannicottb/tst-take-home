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

  def getBestGroupPrices(rates: Seq[Rate], prices: Seq[CabinPrice]): Seq[BestGroupPrice] = {
    // For each cabin + rate group, what is the best (lowest) price?
    val rateCodeToRateGroup = rates.map(r => r.rateCode -> r.rateGroup).toMap
    prices
      .groupBy(p => (p.cabinCode, rateCodeToRateGroup(p.rateCode)))
      .toSeq
      .map {
        case ((_, rateGroup), prices) =>
          prices.minBy(_.price).toBestGroupPrice(rateGroup)
      }
  }

  val expected = List(
    BestGroupPrice("CA", "M1", 200.00, "Military"),
    BestGroupPrice("CA", "S1", 225.00, "Senior"),
    BestGroupPrice("CB", "M1", 230.00, "Military"),
    BestGroupPrice("CB", "S1", 245.00, "Senior")
  )

  def main(args: Array[String]) = {
    println(sampleRates)
    println(samplePrices)
    println("-" * 10)
    val bestPrices = getBestGroupPrices(sampleRates, samplePrices).sortBy(x => (x.cabinCode, x.rateCode))
    println(bestPrices)
//    println(expected)
    if(bestPrices.toSet == expected.toSet) println("PASS")
    else println(s"FAIL: did not match ${expected.toSet}")
  }
}
