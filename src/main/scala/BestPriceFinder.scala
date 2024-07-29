package com.github.gannicottb

import Model.{BestGroupPrice, CabinPrice, Rate}

object BestPriceFinder {
  // For each cabin + rate group, what is the best (lowest) price?
  def getBestGroupPrices(rates: Seq[Rate], prices: Seq[CabinPrice]): Seq[BestGroupPrice] = {
    val rateCodeToRateGroup = rates.map(r => r.rateCode -> r.rateGroup).toMap
    prices
      .groupBy(p => (p.cabinCode, rateCodeToRateGroup(p.rateCode)))
      .toSeq
      .map {
        case ((_, rateGroup), prices) =>
          prices.minBy(_.price).toBestGroupPrice(rateGroup)
      }
  }
}
