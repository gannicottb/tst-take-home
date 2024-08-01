package com.github.gannicottb

import scala.collection.immutable.List
import Model._

object SampleInput {
  val inputRates = List(
    Rate("M1", "Military"),
    Rate("M2", "Military"),
    Rate("S1", "Senior"),
    Rate("S2", "Senior")
  )

  val inputPrices = List(
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

  val inputPromotions = List(
    Promotion("P1", Seq("P3")),
    Promotion("P2", Seq("P4", "P5")),
    Promotion("P3", Seq("P1")),
    Promotion("P4", Seq("P2")),
    Promotion("P5", Seq("P2")),
  )
  val expectedAll = List(
    PromotionCombo(Seq("P1", "P4", "P5")),
    PromotionCombo(Seq("P1", "P2")),
    PromotionCombo(Seq("P2", "P3")),
    PromotionCombo(Seq("P3", "P4", "P5"))
  )
  val expectedP1 = List(
    PromotionCombo(List("P1", "P2")),
    PromotionCombo(List("P1", "P4", "P5"))
  )
  val expectedP3 = List(
    PromotionCombo(List("P3", "P2")),
    PromotionCombo(List("P3", "P4", "P5"))
  )
}
