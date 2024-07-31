package com.github.gannicottb

import Model._

import scala.annotation.tailrec

object Solver {
  // For each cabin + rate group, what is the best (lowest) price?
  // Warning: Can throw if prices include rateCodes not included in the `rates`
  // If that isn't acceptable, could return an Either[CalculationError, Seq[BestGroupPrice]]
  // Could do a sanity check before we do any traversal, but that's wasted
  // effort if the data checks out.
  // Really the problem is string-typing - if we can say that we know the codes and groups ahead of time
  // then we can get compile-time guarantees of no mismatches
  // though we then have to account for runtime errors if we encounter codes/groups we don't know
  // so it really depends on how often new codes/groups enter the third-party system.
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

  def allCombinablePromotions(allPromotions: Seq[Promotion]): Seq[PromotionCombo] =
    allPromotions.foldLeft(Set[PromotionCombo]()){
      case (allCombos, promo) =>
        val result = combinablePromotions(promo.code, allPromotions).toSet
        allCombos ++ result.filter(combo =>
          !allCombos.exists(existing => combo.promotionCodes.toSet.subsetOf(existing.promotionCodes.toSet))
        )
    }.toSeq

  def combinablePromotions(promotionCode: String, allPromotions: Seq[Promotion]): Seq[PromotionCombo] = {
    // Create a hash of code -> Set[NotCombinableCode] for efficient lookup
    val noCombineMap = allPromotions.map(p => p.code -> p.notCombinableWith.toSet).toMap
    // Recursively build the largest possible valid combination of codes
    // for a given starting point and set of possible codes
    @tailrec
    def findCombo(current: Set[String], possibles: Set[String]): Set[String] = {
      if (possibles.isEmpty) {
//        println(s"findCombo: $possibles isEmpty. Return $current")
        current
      } else {
//        println(s"findCombo: $current, $possibles")
        // Add the next possible code and check validity
        val candidate = current + possibles.head
        val isValid   = !candidate.exists(c => noCombineMap(c).intersect(candidate).nonEmpty)
//        println(s"findCombo: $candidate isValid? $isValid")
        // recurse with the rest of the possible codes, adding the next code to current if it's valid
        findCombo(
          if (isValid) candidate else current,
          possibles.tail
        )
      }
    }
    // For each slice of the possible codes, find the largest valid combinations
    // (do not return duplicates or combinations that are subsets of larger combinations
    noCombineMap.keySet.tails.map(_.toSet).foldLeft(Set[Set[String]]()){
      case (combos, possibles) =>
        // find the largest combo starting from our start code, comparing against codes we haven't already exhausted
        val combo = findCombo(Set(promotionCode), possibles -- combos.flatten)
        // Combos of size 1 are not useful to us
        if(combo.size > 1) combos + combo else combos
    }.map(c => PromotionCombo(c.toSeq.sorted)).toSeq
  }
}
