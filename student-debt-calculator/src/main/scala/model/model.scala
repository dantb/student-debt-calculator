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
) {
  def withSalary(salary: Double) = copy(startingSalary = salary)
  def withExtraMonthlyRepayment(repayment: Double) = copy(extraMonthlyRepayment = repayment)
}

// maybe don't need this anymore in Scala 3?
final case class Percentage(value: Double):
  def of(other: Double): Double = (value / 100) * other
  def +(other: Percentage): Percentage = Percentage(value + other.value)

enum Outcome(total: Double, date: LocalDate):
  case WrittenOff(total: Double, date: LocalDate) extends Outcome(total, date)
  case PaidOff(total: Double, date: LocalDate) extends Outcome(total, date)
