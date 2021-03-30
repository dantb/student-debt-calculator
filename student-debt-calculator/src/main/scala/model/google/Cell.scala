//package model.google
//
//import scala.annotation.tailrec
//import scala.util.Try
//
//final case class Cell(row: Row, col: Col) {
//  def apiFormat: String = s"${col.toChar}${row.toInt}"
//
//  def incRow: Cell                   = Cell.incRow(this)
//  def incNRows(n: Int): Option[Cell] = Cell.incNRows(n, this)
//
//  def incCol: Cell                   = Cell.incCol(this)
//  def incNCols(n: Int): Option[Cell] = Cell.incNCols(n, this)
//}
//
//object Cell {
//
//  def incRow(cell: Cell) = Cell(cell.row.inc, cell.col)
//  def incCol(cell: Cell) = Cell(cell.row, cell.col.inc)
//
//  @tailrec
//  def incNRows(n: Int, cell: Cell): Option[Cell] =
//    if (n < 0) None
//    else if (n == 0) Some(cell)
//    else incNRows(n - 1, incRow(cell))
//
//  @tailrec
//  def incNCols(n: Int, cell: Cell): Option[Cell] =
//    if (n < 0) None
//    else if (n == 0) Some(cell)
//    else incNCols(n - 1, incCol(cell))
//
//  def parse(input: String): Option[Cell] =
//    input.splitAt(1) match {
//      case (colStr, rowStr) =>
//        for {
//          // TODO not important but handle cols with multiple characters (e.g. AA, BC)
//          col    <- colStr.headOption.flatMap(Col.fromChar)
//          rowInt <- Try(rowStr.toString.toInt).toOption
//          row    <- Row.fromInt(rowInt)
//        } yield Cell(row, col)
//      case _ => None
//    }
//}
