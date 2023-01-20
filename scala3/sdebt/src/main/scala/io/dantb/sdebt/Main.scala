package io.dantb.sdebt

import cats.effect.{ExitCode, IO, IOApp}

object Main extends IOApp {
  def run(args: List[String]) =
    SdebtServer.stream[IO].compile.drain.as(ExitCode.Success)
}
