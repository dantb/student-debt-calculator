//package model.google
//
//enum Col(value: Int):
//  case A extends Col(1)
//  case B extends Col(2)
//  case C extends Col(3)
//  case D extends Col(4)
//  case E extends Col(5)
//  case F extends Col(6)
//  case G extends Col(7)
//  case H extends Col(8)
//  case I extends Col(9)
//  case J extends Col(10)
//  case K extends Col(11)
//end Col
//
//sealed abstract case class Col(toChar: Char) {
//  def inc: Col = new Col((toChar + 1).toChar) {}
//}
//
//object Col {
//  def fromChar(ch: Char): Option[Col] =
//    if (!ch.isLetter) None else Some(new Col(ch.toUpper) {})
//
//  def a: Col = new Col('A') {}
//  def b: Col = new Col('B') {}
//  def c: Col = new Col('C') {}
//  def d: Col = new Col('D') {}
//  def e: Col = new Col('E') {}
//  def i: Col = new Col('I') {}
//}
