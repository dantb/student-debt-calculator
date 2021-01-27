import calculator._
import model._
import syntax._
import weaver._

import java.time.LocalDate

object CalculatorSpec extends SimpleIOSuite {

  extension (params: Parameters)
    def withSalaryBelowThreshold(amountBelow: Double) = 
      params.withSalary(params.minInterestRateThreshold - amountBelow)
  
  val parameters =
    Parameters(
      startingDate = LocalDate.of(2021, 1, 6),
      startingDebt = 50000,
      startingSalary = 40000,
      yearlySalaryIncrease = 2.percent,
      extraMonthlyRepayment = 100,
      averageUkEarningsGrowth = 1.percent,
      retailPriceIndex = 2.6.percent,
      minInterestRateThreshold = 26840.75,
      maxInterestRateThreshold = 48313.35
    )

  pureTest("Monthly payment should align with the spreadsheet within some tolerance") {
    val result = Calculator.monthlyPayment(parameters)

    assertWithinTolerance(198.69, result, 1.percent)
  }

  pureTest("Monthly payment should only be extra repayment when salary below threshold") {
    val result = Calculator.monthlyPayment(parameters.withSalaryBelowThreshold(1000))

    assertWithinTolerance(100, result, 1.percent)
  }

  pureTest("Salary based interest should align with the spreadsheet within some tolerance") {
    val result = Calculator.salaryBasedInterest(parameters)

    assertWithinTolerance(1.84, result.value, 1.percent)
  }
  
  pureTest("Salary based interest should be 0 when salary below threshold") {
    val result = Calculator.salaryBasedInterest(parameters.withSalaryBelowThreshold(1000))

    assertWithinTolerance(0, result.value, 1.percent)
  }

  pureTest("Monthly accrued interest should align with the spreadsheet within some tolerance") {
    val result = Calculator.monthlyInterestAccrued(parameters)

    assertWithinTolerance(184.93, result, 1.percent)
  }

  pureTest("Monthly accrued interest should only be based on RPI when salary below threshold") {
    val result = Calculator.monthlyInterestAccrued(parameters.withSalaryBelowThreshold(1000))
    val expected = parameters.retailPriceIndex.of(parameters.startingDebt) / Constants.MonthsInYear

    assertWithinTolerance(expected, result, 1.percent)
  }

  def assertWithinTolerance(expected: Double, actual: Double, tolerance: Percentage): Expectations =
    expect(math.abs(expected - actual) <= tolerance.of(expected))

}
