import model._

import java.time.LocalDate
import syntax._

class CalculatorSpec extends munit.FunSuite {
  
  test(
    "Calculator should return align with the spreadsheet within some tolerance") {
    val parameters =
      Parameters(
        startingDate = LocalDate.of(2021, 1, 6),
        startingDebt = 50000,
        startingSalary = 40000,
        2.percent.get,
        extraMonthlyRepayment = 100,
        1.percent.get,
        2.6.percent.get,
        minInterestRateThreshold = 26840.75,
        maxInterestRateThreshold = 48313.35
      )
      
    val result = Calculator.computeMonthlyPayment(parameters)
    
    assertWithinTolerance(204.69, result, 1)
  }
  
  
  def assertWithinTolerance(expected: Double, actual: Double, tolerance: Double): Unit =
    assert(math.abs(expected - actual) <= tolerance)
    
}
