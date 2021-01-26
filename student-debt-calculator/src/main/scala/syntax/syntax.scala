package syntax

import model.Percentage

extension (c: Double)
  def percent: Option[Percentage] = Percentage(c)
