package com.jacobshao.arbitrage

import cats.Show
import cats.effect.ExitCode
import cats.implicits._
import monix.eval.{Task, TaskApp}
import org.http4s.EntityDecoder
import org.http4s.circe._
import org.http4s.client.blaze.BlazeClientBuilder
import org.http4s.implicits._

case class Currency(value: String) extends AnyVal

case class Rate(value: Double) extends AnyVal

case class ExchangeRate(from: Currency, to: Currency, rate: Rate)

object Main extends TaskApp {

  implicit val responseJsonDecoder: EntityDecoder[Task, Map[String, Double]] = jsonOf[Task, Map[String, Double]]
  implicit val showMap: Show[Map[String, Double]] = Show.show(hashMap => hashMap.foldLeft("") {
    case (acc, kv) => acc + s"${kv._1}: ${kv._2}\n"
  })

  override def run(args: List[String]): Task[ExitCode] = {
    BlazeClientBuilder(scheduler).resource
      .use { client =>
        for {
          resBody <- client.expect[Map[String, Double]](uri"https://fx.priceonomics.com/v1/rates/")
          _ <- Task.now(println(resBody.show))
          encoded <- Task.now(resBody.foldLeft(Seq[ExchangeRate]()) {
            case (acc, (key, value)) =>
              val currencyPair = key.split("_")
              val from = Currency(currencyPair(0))
              val to = Currency(currencyPair(1))
              val rate = Rate(value)
              acc :+ ExchangeRate(from, to, rate)
          })
          exitCode <- Task.now(println("finished"))
            .as(ExitCode.Success)
        } yield exitCode
      }.onErrorHandleWith { t =>
      Task(println(s"Fatal failure: ${t.getMessage}")) *> Task.pure(ExitCode.Error)
    }

  }
}
