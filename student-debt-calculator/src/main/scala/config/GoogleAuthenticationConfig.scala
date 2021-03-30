package config

import cats._
import cats.syntax.all.given
import config.ReadEnvironment

final case class GoogleAuthenticationConfig(
  baseUri: String,
  profileId: String,
  privateKeyId: String,
  privateKey: String,
  clientEmail: String,
  clientId: String
)

object GoogleAuthenticationConfig {

  def fromEnvironment[F[_]: Monad: [X[_]] =>> ReadEnvironment[X, String]]: F[GoogleAuthenticationConfig] =
    for {
      baseUri      <- "GoogleAuthBaseUri".readEnv
      profileId    <- "GoogleProfileId".readEnv
      privateKeyId <- "GooglePrivateKeyId".readEnv
      clientEmail  <- "GoogleServiceAccount".readEnv
      privateKey   <- "GoogleRSAPrivateKey".readEnv
      clientId     <- "GoogleClientId".readEnv
    } yield GoogleAuthenticationConfig(baseUri, profileId, privateKeyId, privateKey, clientEmail, clientId)

}
