package model

import java.time.LocalDate

final case class Parameters(
  startingDate: LocalDate,
  startingDebt: Double,
  startingSalary: Double,
  yearlySalaryIncrease: Percentage,
  extraMonthlyRepayment: Double,
  averageUkEarningsGrowth: Percentage,
  retailPriceIndex: Percentage,
  minInterestRateThreshold: Double,
  maxInterestRateThreshold: Double
)

// maybe don't need this anymore in Scala 3?
sealed abstract case class Percentage(value: Double):
  def of(other: Double): Double = (value / 100) * other

object Percentage:
  def apply(value: Double): Option[Percentage] = 
    if (value >=0 && value <= 100) Some(new Percentage(value) {}) else None
