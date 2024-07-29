package com.github.gannicottb

object Model {
  case class Rate(rateCode: String, rateGroup: String)

  case class CabinPrice(cabinCode: String, rateCode: String, price: BigDecimal) {
    def toBestGroupPrice(rateGroup: String) =
      BestGroupPrice(cabinCode, rateCode, price, rateGroup)
  }

  case class BestGroupPrice(cabinCode: String, rateCode: String, price: BigDecimal, rateGroup: String)
}
