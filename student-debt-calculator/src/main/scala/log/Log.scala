package log

import cats.effect._

trait Log[F[_]]:
  def info(message: String): F[Unit]
  def error(message: String): F[Unit]
  extension (message: String) def logInfo: F[Unit] = info(message)
  extension (message: String) def logError: F[Unit] = error(message)

object Log:
  given printLog[F[_]: Sync]: Log[F] with
    def info(message: String): F[Unit] = Sync[F].delay(println(s"INFO: $message"))
    def error(message: String): F[Unit] = Sync[F].delay(println(s"ERROR: $message"))
