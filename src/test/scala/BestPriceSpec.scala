package com.github.gannicottb

import Model.{ BestGroupPrice, CabinPrice, Rate }

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class BestPriceSpec extends AnyWordSpec with Matchers {
  "BestPriceFinder" should {
    val devRate = Rate("D", "Developer")
    "return no prices" when {
      "no prices given" in {
        BestPriceFinder.getBestGroupPrices(Seq(devRate), Seq()) shouldBe empty
      }
    }
    "return the only price" when {
      "only one cabinPrice is provided" in {
        val price = CabinPrice("CA", devRate.rateCode, 1.00)
        BestPriceFinder.getBestGroupPrices(Seq(devRate), Seq(price)) shouldBe Seq(
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
        BestPriceFinder.getBestGroupPrices(Seq(devRate), prices) shouldBe Seq(
          prices.head.toBestGroupPrice(devRate.rateGroup)
        )
      }
      "there are multiple codes per group" in {
        val internDevRate = Rate("DI", devRate.rateGroup)
        val prices = Seq(
          CabinPrice("CA", devRate.rateCode, 1.00),
          CabinPrice("CA", internDevRate.rateCode, 0.50),
        )
        BestPriceFinder.getBestGroupPrices(Seq(devRate, internDevRate), prices) shouldBe Seq(
          prices.last.toBestGroupPrice(devRate.rateGroup)
        )
      }
    }
  }
}
