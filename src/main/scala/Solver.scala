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
    val noCombineMap = allPromotions.map(p => p.code -> p.notCombinableWith.toSet).toMap
    @tailrec
    def findCombo(current: Set[String], possibles: Set[String]): Set[String] = {
      if (possibles.isEmpty) {
//        println(s"findCombo: $possibles isEmpty. Return $current")
        current
      } else {
//        println(s"findCombo: $current, $possibles")
        val candidate = current + possibles.head
        val isValid   = !candidate.exists(c => noCombineMap(c).intersect(candidate).nonEmpty)
//        println(s"findCombo: $candidate isValid? $isValid")
        findCombo(
          if (isValid) candidate else current,
          possibles.tail
        )
      }
    }

    noCombineMap.keySet.tails.map(_.toSet).foldLeft(Set[Set[String]]()){
      case (combos, possibles) =>
        val combo = findCombo(Set(promotionCode), possibles -- combos.flatten)
        if(combo.size > 1) combos + combo else combos
    }.map(c => PromotionCombo(c.toSeq.sorted)).toSeq
  }

  def promotionComboIsValid(combo: PromotionCombo, notCombinableMap: Map[String, Seq[String]]): Boolean = {
    val codeSet = combo.promotionCodes.toSet
    // If there is any code in this combo that is incompatible with any other member of the combo, it's invalid
    val invalid = codeSet.exists(c => notCombinableMap(c).toSet.intersect(codeSet).nonEmpty)
    !invalid
  }
}
