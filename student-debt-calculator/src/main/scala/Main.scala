import cats.effect._
import cats.syntax.all.given
import config.ReadEnvironment
import com.google.auth.oauth2.{AccessToken, GoogleCredentials}
import google.{BuildGoogleCredentials, GoogleAuthenticationConfig}
import log.Log

object Main extends cats.effect.IOApp {
  
  def run(args: List[String]): IO[ExitCode] = {

    given log: Log[IO] = Log.printLog[IO]
    
    val program: IO[ExitCode] = for {
      config      <- GoogleAuthenticationConfig.fromEnvironment[IO]
      credentials <- BuildGoogleCredentials.create[IO].build(config)
      token       <- getAccessToken(credentials)
      _           <- s"Token is $token".logInfo
    } yield ExitCode.Success

    program.recoverWith { case e => IO(println(s"Failed with $e")).as(ExitCode.Error) }
  }

  def getAccessToken(creds: GoogleCredentials): IO[AccessToken] =
    IO {
      creds.refreshIfExpired()
      creds.getAccessToken
    }

}
