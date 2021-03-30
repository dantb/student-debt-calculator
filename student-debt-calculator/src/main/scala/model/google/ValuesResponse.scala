//package model.google
//
//import cats.effect.Sync
//import io.circe.Decoder
//import org.http4s.EntityDecoder
//
//import scala.annotation.tailrec
//
//final case class ValuesResponse(range: Range, values: Map[Cell, String])
//
//object ValuesResponse {
//
//  implicit val decoder: Decoder[ValuesResponse] = Decoder.instance { cur =>
//    for {
//      range  <- cur.get[Range]("range")
//      values <- cur.get[List[List[String]]]("values")
//    } yield ValuesResponse(range, parseCells(range.start, values))
//  }
//
//  // each inner list is one row
//  private def parseCells(startOfRange: Cell, theCells: List[List[String]]): Map[Cell, String] = {
//
//    @tailrec
//    def loop(seedCell: Cell, cells: List[List[String]], acc: Map[Cell, String]): Map[Cell, String] =
//      cells match {
//        case currentRow :: otherRows => loop(seedCell.incRow, otherRows, acc ++ parseRow(seedCell, currentRow))
//        case Nil                     => acc
//      }
//
//    loop(startOfRange, theCells, Map.empty)
//  }
//
//  private def parseRow(seedCell: Cell, theCols: List[String]): Map[Cell, String] = {
//
//    @tailrec
//    def loop(seed: Cell, cols: List[String], acc: Map[Cell, String]): Map[Cell, String] =
//      cols match {
//        case cellValue :: cellValues => loop(seed.incCol, cellValues, acc + (seed -> cellValue))
//        case Nil                     => acc
//      }
//
//    loop(seedCell, theCols, Map.empty)
//  }
//
//  implicit def ed[F[_]: Sync]: EntityDecoder[F, ValuesResponse] = org.http4s.circe.jsonOf[F, ValuesResponse]
//}
