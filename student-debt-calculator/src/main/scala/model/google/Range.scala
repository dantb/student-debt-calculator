//package model.google
//
//import io.circe.Decoder
//
//final case class Range(sheetTitle: Option[String], start: Cell, end: Cell) {
//  def apiFormat: String =
//    s"${sheetTitle.map(st => s"$st!").getOrElse("")}${start.apiFormat}:${end.apiFormat}"
//}
//
//object Range {
//
//  implicit val decoder: Decoder[Range] =
//    Decoder.decodeString.emap(parse(_).toRight("Incorrect google sheets range shape, should be \"Sheet1!A1:F6\""))
//
//  def parse(input: String): Option[Range] =
//    input.split('!').toList match {
//      case sheetTitle :: cells :: Nil => parsePair(Some(sheetTitle), cells)
//      case cells :: Nil               => parsePair(None, cells)
//      case _                          => None
//    }
//
//  def parsePair(title: Option[String], input: String): Option[Range] =
//    input.split(':').toList match {
//      case start :: end :: Nil =>
//        for {
//          startCell <- Cell.parse(start)
//          endCell   <- Cell.parse(end)
//        } yield Range(title, startCell, endCell)
//      case start :: Nil =>
//        for {
//          startCell <- Cell.parse(start)
//        } yield Range(title, startCell, startCell)
//      case _ => None
//    }
//}
