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

  trait ValidCombo {
    def codes: Set[String]
    def insertIfValid(code: String): ValidCombo
    def toPromotionCombo: PromotionCombo =
      PromotionCombo(codes.toSeq.sorted)
  }
  final class ValidComboImpl(noCombineMap: Map[String, Set[String]], combo: Set[String] = Set()) extends ValidCombo {
    override def codes: Set[String] = combo
    override def insertIfValid(code: String): ValidCombo = {
      val candidate = combo + code
      val isValid   = !candidate.exists(c => noCombineMap(c).intersect(candidate).nonEmpty)
      new ValidComboImpl(noCombineMap, if (isValid) candidate else combo)
    }
  }

  private def largestValidCombosForCode(promotionCode: String, allPromotions: Seq[Promotion]): Set[ValidCombo] = {
    // Recursively build the largest possible valid combination of codes
    // for a given starting point and set of possible codes
    @tailrec
    def findLargestCombo(current: ValidCombo, possibles: Set[String]): Option[ValidCombo] =
      possibles.headOption match {
        // Ran out of codes to try, return current (if it has more than one code)
        case None if current.codes.size > 1 => Some(current)
        // There's another code to try, so recurse with that
        case Some(nextPossible) =>
          findLargestCombo(
            current.insertIfValid(nextPossible),
            possibles.tail // could throw but won't because the set is nonEmpty by definition here
          )
        case _ => None
      }

    // Create a hash of code -> Set[NotCombinableCode] for efficient lookup
    val noCombineMap = allPromotions.map(p => p.code -> p.notCombinableWith.toSet).toMap
    // For each slice of the possible codes, find the largest valid combinations
    // (do not return duplicates or combinations that are subsets of larger combinations)
    noCombineMap.keySet.tails
      .map(_.toSet)
      .foldLeft(Set[ValidCombo]()) {
        case (combos, possibles) =>
          // find the largest combo starting from our start code, comparing against codes we haven't already exhausted
          findLargestCombo(new ValidComboImpl(noCombineMap, Set(promotionCode)), possibles -- combos.flatMap(_.codes))
            .fold(combos)(combos + _)
      }
  }

  def combinablePromotions(promotionCode: String, allPromotions: Seq[Promotion]): Seq[PromotionCombo] =
    largestValidCombosForCode(promotionCode, allPromotions)
      .map(_.toPromotionCombo)
      .toSeq

  // Given a list of promotions that may not be combinable with others:
  // find largest valid combinations for each code
  def allCombinablePromotions(allPromotions: Seq[Promotion]): Seq[PromotionCombo] =
    allPromotions
      .foldLeft(Set[ValidCombo]()) {
        case (allCombos, promo) =>
          // Find the set of largest combos for the current code, then add it to the
          // result set, removing any results that are subsets of combos in the result set
          allCombos ++ largestValidCombosForCode(promo.code, allPromotions)
            .filterNot(
              combo => allCombos.exists(existing => combo.codes.subsetOf(existing.codes))
            )
      }
      .map(_.toPromotionCombo)
      .toSeq
}
