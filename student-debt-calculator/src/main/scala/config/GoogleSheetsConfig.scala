package config

import cats._
import cats.syntax.all.given
import config.ReadEnvironment
import sttp.client3._

final case class GoogleSheetsConfig(apiKey: String) {
  val baseUrl = uri"https://sheets.googleapis.com/v4/spreadsheets"
}

object GoogleSheetsConfig {

  def fromEnvironment[F[_]: Functor](using ReadEnvironment[F, String]): F[GoogleSheetsConfig] =
    "GoogleSheetsApiKey".readEnv.map(GoogleSheetsConfig(_))

}
