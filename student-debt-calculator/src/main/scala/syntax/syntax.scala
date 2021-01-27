package syntax

import model.Percentage

extension (c: Double)
  def percent: Percentage = Percentage(c)
