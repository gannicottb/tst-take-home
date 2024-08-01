package com.github.gannicottb

import Model.{BestGroupPrice, CabinPrice, Promotion, PromotionCombo, Rate}

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class SolverSpec extends AnyWordSpec with Matchers {
  "BestPriceFinder" should {
    val devRate = Rate("D", "Developer")
    "return no prices" when {
      "no prices given" in {
        Solver.getBestGroupPrices(Seq(devRate), Seq()) shouldBe empty
      }
    }
    "return the only price" when {
      "only one cabinPrice is provided" in {
        val price = CabinPrice("CA", devRate.rateCode, 1.00)
        Solver.getBestGroupPrices(Seq(devRate), Seq(price)) shouldBe Seq(
          price.toBestGroupPrice(devRate.rateGroup)
        )
      }
    }
    "return the lowest price for each cabin + rate group combination" when {
      "there is only one cabin + rate group combination" in {
        val prices = Seq(
          CabinPrice("CA", devRate.rateCode, 1.00),
          CabinPrice("CA", devRate.rateCode, 10.00)
        )
        Solver.getBestGroupPrices(Seq(devRate), prices) shouldBe Seq(
          prices.head.toBestGroupPrice(devRate.rateGroup)
        )
      }
      "there are multiple codes per group" in {
        val internDevRate = Rate("DI", devRate.rateGroup)
        val prices = Seq(
          CabinPrice("CA", devRate.rateCode, 1.00),
          CabinPrice("CA", internDevRate.rateCode, 0.50),
        )
        Solver.getBestGroupPrices(Seq(devRate, internDevRate), prices) shouldBe Seq(
          prices.last.toBestGroupPrice(devRate.rateGroup)
        )
      }
      "it's the given use case" in {
        val res = Solver.getBestGroupPrices(SampleInput.inputRates, SampleInput.inputPrices)
        res.toSet shouldBe SampleInput.expected.toSet
      }
    }
  }
  "PromotionComboSolver" should {
    def shouldBeEquivalent(l: Seq[PromotionCombo], r: Seq[PromotionCombo]) = {
      def toSetOfSets(s: Seq[PromotionCombo]) = s.map(_.promotionCodes.toSet).toSet
       toSetOfSets(l) shouldBe toSetOfSets(r)
    }

    val promotions = SampleInput.inputPromotions
    "find combos for given codes" in {
      val res = Solver.combinablePromotions("P1", promotions)
      val res2 = Solver.combinablePromotions("P2", promotions)
      val res3 = Solver.combinablePromotions("P3", promotions)
      shouldBeEquivalent(res, SampleInput.expectedP1)
      shouldBeEquivalent(res2, Seq(PromotionCombo(Seq("P1", "P2")), PromotionCombo(Seq("P2", "P3"))))
      shouldBeEquivalent(res3, SampleInput.expectedP3)
    }
    "find all combos" in {
      val res = Solver.allCombinablePromotions(promotions)
      shouldBeEquivalent(res, SampleInput.expectedAll)
    }
    "find nothing if all combos are uncombinable" in {
      val allCodes = Seq("P1", "P2", "P3", "P4", "P5")
      val promotions = allCodes.map(c =>
        Promotion(c, allCodes.filterNot(_ == c))
      )
      val res = Solver.allCombinablePromotions(promotions)
      res.size shouldBe 0
    }
    "find one combo containing all codes if all codes are combinable" in {
      val allCodes = Seq("P1", "P2", "P3", "P4", "P5")
      val promotions = allCodes.map(c =>
        Promotion(c, Seq())
      )
      val res = Solver.allCombinablePromotions(promotions)
      shouldBeEquivalent(res, Seq(PromotionCombo(allCodes)))
    }
  }
}
