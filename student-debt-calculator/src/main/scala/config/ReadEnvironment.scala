package config

import cats.effect._
import cats._
import cats.syntax.all.given

trait ReadEnvironment[F[_], A]:
  def read(key: String): F[A]
  extension (key: String) def readEnv: F[A] = read(key) 

object ReadEnvironment {
  
  given readString[F[_]: Sync]: ReadEnvironment[F, String] with
    def read(key: String): F[String] = 
      Sync[F].delay(sys.env.get(key))
        .flatMap(MonadError[F, Throwable].fromOption(_, new Exception(s"Key $key is not in the environment")))
}
