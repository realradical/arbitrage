package com.jacobshao.arbitrage

import cats.Show
import cats.effect.ExitCode
import cats.implicits._
import io.circe.generic.extras.semiauto.deriveUnwrappedDecoder
import io.circe.{Decoder, KeyDecoder}
import monix.eval.{Task, TaskApp}
import org.http4s.EntityDecoder
import org.http4s.circe._
import org.http4s.client.blaze.BlazeClientBuilder
import org.http4s.implicits._

import scala.math.log

case class CurrencyPair(value: String) extends AnyVal

case class Currency(value: String) extends AnyVal

case class Rate(value: Double) extends AnyVal

case class NegativeLogRate(value: Double) extends AnyVal

case class Edge(from: Currency, to: Currency, negativeLogRate: NegativeLogRate)

case class Graph(vertices: Map[Currency, Seq[Edge]])

object Main extends TaskApp {

  implicit val currencyKeyDecoder: KeyDecoder[CurrencyPair] = (key: String) => Some(CurrencyPair(key))
  implicit val rateDecoder: Decoder[Rate] = deriveUnwrappedDecoder
  implicit val responseJsonDecoder: EntityDecoder[Task, Map[CurrencyPair, Rate]] = jsonOf[Task, Map[CurrencyPair, Rate]]

  val source: Currency = Currency("USD")

  implicit val showMap: Show[Map[CurrencyPair, Rate]] = Show.show(hashMap => hashMap.foldLeft("") {
    case (acc, kv) => acc + f"${kv._1.value}: ${kv._2.value}%.7f\n"
  })

  override def run(args: List[String]): Task[ExitCode] = {
    BlazeClientBuilder(scheduler).resource
      .use { client =>
        for {
          resBody <- client.expect[Map[CurrencyPair, Rate]](uri"https://fx.priceonomics.com/v1/rates/")
          _ <- Task.now(println(resBody.show))
          graph <- convertResponseToGraph(resBody)
          _ <- findArbitrage(graph)
          exitCode <- Task.unit
            .as(ExitCode.Success)
        } yield exitCode
      }.onErrorHandleWith { t =>
      Task(println(s"Fatal failure: ${t.getMessage}")) *> Task.pure(ExitCode.Error)
    }
  }

  private def convertResponseToGraph(map: Map[CurrencyPair, Rate]): Task[Graph] =
    Task {
      map.foldLeft(Graph(Map.empty)) {
        case (acc, (k, v)) =>
          val currencyPair = k.value.split("_")
          val from = Currency(currencyPair(0))
          val to = Currency(currencyPair(1))
          val negativeLogRate = NegativeLogRate(-log(v.value))
          val edge = Edge(from, to, negativeLogRate)
          val updatedVertices = acc.vertices.get(from) match {
            case Some(v) => acc.vertices + (from -> (v :+ edge))
            case None => acc.vertices + (from -> List(edge))
          }
          Graph(updatedVertices)
      }
    }

  case class Distance(value: Double) extends AnyVal

  type Predecessor = Option[Currency]

  case class VertexDistance(distance: Distance, predecessor: Predecessor)

  type VertexDistanceMap = Map[Currency, VertexDistance]

  private def findArbitrage(graph: Graph): Task[Unit] =
    Task {
      val distanceMap: VertexDistanceMap = graph.vertices.map {
        case (currency, _) =>
          val d: Distance = if (currency == source) Distance(0d) else Distance(Double.PositiveInfinity)
          val p: Predecessor = None
          (currency, VertexDistance(d, p))
      }
      val iterations = graph.vertices.keys.size - 1
      val allEdges: Seq[Edge] = graph.vertices.values.flatten.toSeq

      val relaxedDistanceMap = (1 to iterations).foldLeft(distanceMap) {
        case (acc, _) => relaxEdges(acc, allEdges)
      }

      lastRelaxEdges(relaxedDistanceMap, allEdges)
    }

  private def relaxEdges(distanceMap: VertexDistanceMap, edges: Seq[Edge]): VertexDistanceMap =
    edges.foldLeft(distanceMap) {
      case (acc, edge) =>
        val from = edge.from
        val to = edge.to
        val weight = edge.negativeLogRate.value

        (for {
          fromVertexDistance <- distanceMap.get(from)
          toVertexDistance <- distanceMap.get(to)
        } yield {
          val newWeight = fromVertexDistance.distance.value + weight
          val toWeight = toVertexDistance.distance.value

          if (newWeight < toWeight) {
            distanceMap + (to -> VertexDistance(Distance(newWeight), Some(from)))
          } else {
            acc
          }
        }).getOrElse(acc)
    }

  private def lastRelaxEdges(distanceMap: VertexDistanceMap, edges: Seq[Edge]): VertexDistanceMap =
    edges.foldLeft(distanceMap) {
      case (acc, edge) =>
        val from = edge.from
        val to = edge.to
        val weight = edge.negativeLogRate.value

        (for {
          fromVertexDistance <- distanceMap.get(from)
          toVertexDistance <- distanceMap.get(to)
        } yield {
          val newWeight = fromVertexDistance.distance.value + weight
          val toWeight = toVertexDistance.distance.value

          if (newWeight < toWeight) {
            var arbCycle = List(from, to)
            var currentVertex = from
            while (continueFindPredecessor(distanceMap, currentVertex, arbCycle)) {
              val predecessor = distanceMap.get(currentVertex).flatMap(_.predecessor).get
              arbCycle = predecessor +: arbCycle
              currentVertex = predecessor
            }
            val startCurrency = distanceMap.get(currentVertex).flatMap(_.predecessor).get
            if (startCurrency == source && arbCycle.last != source) arbCycle = arbCycle :+ source
            arbCycle = startCurrency +: arbCycle
            println("Arbitrage Opportunity:")
            println(arbCycle.mkString(" -> "))
            acc
          } else {
            acc
          }
        }).getOrElse(acc)
    }

  private def continueFindPredecessor(distanceMap: VertexDistanceMap, currentVertex: Currency, arbCycle: Seq[Currency]): Boolean =
    distanceMap.get(currentVertex).flatMap(pre =>
      pre.predecessor.map {
        case currency if currency == source => false
        case currency if !arbCycle.contains(currency) => true
        case _ => false
      }).getOrElse(false)
}


