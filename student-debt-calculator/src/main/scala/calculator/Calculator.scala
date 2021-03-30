package calculator

import model._
import syntax._
import Constants._

object Calculator:

  def monthlyPayment(parameters: Parameters): Double =
    import parameters._
    val salaryAboveThreshold = startingSalary - minInterestRateThreshold
    println(s"------- Salary above threshold: $salaryAboveThreshold")
    if (salaryAboveThreshold <= 0) extraMonthlyRepayment
    else (PercentageOfEarningsOverThreshold.of(salaryAboveThreshold) / MonthsInYear) + extraMonthlyRepayment

  def monthlyInterestAccrued(parameters: Parameters): Double =
    (parameters.retailPriceIndex + salaryBasedInterest(parameters)).of(parameters.startingDebt) / MonthsInYear

  // It's a slider from the bottom of the threshold, to the top at 3%.
  def salaryBasedInterest(parameters: Parameters): Percentage =
    import parameters._
    val salaryAboveThreshold = startingSalary - minInterestRateThreshold
    if (salaryAboveThreshold > 0) {
      val proportionIntoThreshold: Double =
        salaryAboveThreshold / (maxInterestRateThreshold - minInterestRateThreshold)
      Percentage(math.min(proportionIntoThreshold * 3, 3))
    } else 0.percent

