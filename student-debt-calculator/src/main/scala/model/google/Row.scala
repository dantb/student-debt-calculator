//package model.google
//
//sealed abstract case class Row(toInt: Int) {
//  def inc: Row = new Row(toInt + 1) {}
//}
//
//object Row {
//  def fromInt(n: Int): Option[Row] =
//    if (n < 1) None else Some(new Row(n) {})
//
//  def one: Row   = new Row(1)  {}
//  def two: Row   = new Row(2)  {}
//  def three: Row = new Row(3)  {}
//  def four: Row  = new Row(4)  {}
//  def five: Row  = new Row(5)  {}
//  def six: Row   = new Row(6)  {}
//  def seven: Row = new Row(7)  {}
//  def eight: Row = new Row(8)  {}
//  def nine: Row  = new Row(9)  {}
//  def ten: Row   = new Row(10) {}
//}
