package google

import cats.effect.Sync
import com.google.auth.oauth2.GoogleCredentials
import io.circe.syntax.given

import java.io.ByteArrayInputStream
import java.nio.charset.StandardCharsets
import io.circe.{Json, Printer}

trait BuildGoogleCredentials[F[_]]:
  def build(gaConfig: GoogleAuthenticationConfig): F[GoogleCredentials]

object BuildGoogleCredentials:

  def apply[F[_]](using b: BuildGoogleCredentials[F]) = b

  given create[F[_]: Sync]: BuildGoogleCredentials[F] with
    def build(gaConfig: GoogleAuthenticationConfig): F[GoogleCredentials] =
      // not sure if Java library is launching any missiles, suspending just to be safe
      Sync[F].delay(
        GoogleCredentials
          .fromStream(new ByteArrayInputStream(buildCredentialsFile(gaConfig)))
          .createScoped("https://www.googleapis.com/auth/spreadsheets")
      )

  def buildCredentialsFile(gaConfig: GoogleAuthenticationConfig): Array[Byte] =
    Json
      .obj(
        "type"           -> "service_account".asJson,
        "private_key_id" -> gaConfig.privateKeyId.asJson,
        "private_key"    -> gaConfig.privateKey.replace("\\n", System.lineSeparator()).asJson,
        "client_email"   -> gaConfig.clientEmail.asJson,
        "client_id"      -> gaConfig.clientId.asJson
      )
      .printWith(Printer.noSpaces)
      .getBytes(StandardCharsets.UTF_8)
