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
    }
  }
  "PromotionComboSolver" should {
    val promotions = Seq(
      Promotion("P1", Seq("P3")),
      Promotion("P2", Seq("P4", "P5")),
      Promotion("P3", Seq("P1")),
      Promotion("P4", Seq("P2")),
      Promotion("P5", Seq("P2")),
    )
    val notCombinableMap = promotions.map(p => p.code -> p.notCombinableWith).toMap
    "determine validity" when {
      "combo is valid" in {
        Solver.promotionComboIsValid(
          PromotionCombo(Seq("P1", "P2")),
          notCombinableMap
        ) shouldBe true

        Solver.promotionComboIsValid(
          PromotionCombo(Seq("P1", "P4", "P5")),
          notCombinableMap
        ) shouldBe true
        Solver.promotionComboIsValid(
          PromotionCombo(Seq("P2", "P3")),
          notCombinableMap
        ) shouldBe true
      }
      "combo is invalid" in {
        Solver.promotionComboIsValid(
          PromotionCombo(Seq("P1", "P3")),
          notCombinableMap
        ) shouldBe false

        Solver.promotionComboIsValid(
          PromotionCombo(Seq("P2", "P4", "P5")),
          notCombinableMap
        ) shouldBe false

        Solver.promotionComboIsValid(
          PromotionCombo(Seq("P1", "P3")),
          notCombinableMap
        ) shouldBe false
      }
    }
//    "find combos" in {
//      val res = BestPriceFinder.combinablePromotions("P1", promotions)
//      val res2 = BestPriceFinder.combinablePromotions("P2", promotions)
//      val res3 = BestPriceFinder.combinablePromotions("P3", promotions)
//      println(res.toSet) // hack to eliminate redundant results
//      println(res2.toSet) // hack to eliminate redundant results
//      println(res3.toSet) // hack to eliminate redundant results
//      res.toSet shouldBe Set(PromotionCombo(List("P1", "P2")), PromotionCombo(List("P1", "P4", "P5")))
//      res2.toSet shouldBe Set(PromotionCombo(List("P2", "P1")))
//      res3.toSet shouldBe Set(PromotionCombo(List("P3", "P2")), PromotionCombo(List("P3", "P4", "P5")))
//    }
    "find combos_" in {
      val res = Solver.combinablePromotions("P1", promotions)
      val res2 = Solver.combinablePromotions("P2", promotions)
      val res3 = Solver.combinablePromotions("P3", promotions)
//      println("combos_")
//      println(res.toSet) // hack to eliminate redundant results
//      println(res2.toSet) // hack to eliminate redundant results
//      println(res3.toSet) // hack to eliminate redundant results
//      res.toSet shouldBe Set(PromotionCombo(List("P1", "P2")), PromotionCombo(List("P1", "P4", "P5")))
//      res2.toSet shouldBe Set(PromotionCombo(List("P2", "P1")))
//      res3.toSet shouldBe Set(PromotionCombo(List("P3", "P2")), PromotionCombo(List("P3", "P4", "P5")))
    }
//    "find combos__" in {
//      val res = BestPriceFinder.combinablePromotions__("P1", promotions)
////      val res2 = BestPriceFinder.combinablePromotions__("P2", promotions)
////      val res3 = BestPriceFinder.combinablePromotions__("P3", promotions)
//      println("combos__")
//      println(res.toSet) // hack to eliminate redundant results
////      println(res2.toSet) // hack to eliminate redundant results
////      println(res3.toSet) // hack to eliminate redundant results
//      //      res.toSet shouldBe Set(PromotionCombo(List("P1", "P2")), PromotionCombo(List("P1", "P4", "P5")))
//      //      res2.toSet shouldBe Set(PromotionCombo(List("P2", "P1")))
//      //      res3.toSet shouldBe Set(PromotionCombo(List("P3", "P2")), PromotionCombo(List("P3", "P4", "P5")))
//    }
    "find all combos" in {
      val res = Solver.allCombinablePromotions(promotions)
      res.toSet shouldBe Set(
        PromotionCombo(Seq("P1", "P4", "P5")),
        PromotionCombo(List("P1", "P2")),
        PromotionCombo(List("P2", "P3")),
        PromotionCombo(List("P3", "P4", "P5"))
      )
//      println(res)
    }
  }
}
