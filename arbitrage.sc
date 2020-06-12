import $ivy.{`org.http4s::http4s-blaze-client:0.21.1`, `org.http4s::http4s-circe:0.21.1`, `io.circe::circe-generic:0.13.0`, `io.circe::circe-generic-extras:0.13.0`, `io.monix::monix:3.1.0`}

import cats.Show
import cats.effect.ExitCode
import cats.implicits._
import io.circe.generic.extras.semiauto.deriveUnwrappedDecoder
import io.circe.{Decoder, KeyDecoder}
import monix.eval.{Task, TaskApp}
import monix.execution.Scheduler.Implicits.global
import org.http4s.EntityDecoder
import org.http4s.circe._
import org.http4s.client.blaze.BlazeClientBuilder
import org.http4s.implicits._

import scala.math.{log, E, pow}

/** The goal is to find a sequence where the starting currency should be the same as the last currency and
 * Rate(currency_1, currency_2) * Rate(currency_2, currency_3) * ... * Rate(currency_n, currency_1) > 1
 * We can then apply log on both sides, so we get:
 * logRate(currency_1, currency_2) + logRate(currency_2, currency_3) + ... + logRate(currency_n, currency_1) > 0
 * --> -logRate(currency_1, currency_2) - logRate(currency_2, currency_3) - ... - logRate(currency_n, currency_1) < 0
 * Now the problem becomes finding negative cycles in a directed graph where vertices are the currencies and the edge weight
 * is the negative log of the exchange rate for each currency pair.
 * We can use Bellman Ford algorithm.
 *
 * Regarding the algorithmic complexity, assuming an exchange rate exists for every pair of currencies, in other words,
 * we are working with a complete graph. The time complexity is O(n^3) where n is the number of currencies. Because the
 * algorithm needs to iterate the total number of vertices -1 times and in each iteration, it needs to relax all edges.
 * O((V-1) - E) ~= O(V*E) ~= O(V* V*(V-1)/2) ~= O(V^3). The space complexity is O(n) where n is the number of currencies.
 * Because we need to store distance and predecessor for each vertex.
 */

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
          _ <- Task(println(resBody.show))
          graph <- convertResponseToGraph(resBody)
          arbitrages <- findArbitrage(graph)
          exitCode <- printReport(arbitrages).as(ExitCode.Success)
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

  case class Arbitrage(sequence: List[Currency], finalAmount: Double)

  type Arbitrages = Seq[Arbitrage]

  private def findArbitrage(graph: Graph): Task[Arbitrages] =
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

      detectArbitrageSequence(relaxedDistanceMap, allEdges)
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

  private def detectArbitrageSequence(distanceMap: VertexDistanceMap, edges: Seq[Edge]): Arbitrages = {
    edges.foldLeft(List.empty[Arbitrage]) {
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
            var arbitrageSequence = List(from, to)
            var currentVertex = from
            while (continueFindPredecessor(distanceMap, currentVertex, arbitrageSequence)) {
              val predecessor = distanceMap.get(currentVertex).flatMap(_.predecessor).get
              arbitrageSequence = predecessor +: arbitrageSequence
              currentVertex = predecessor
            }
            arbitrageSequence = distanceMap.get(currentVertex).flatMap(_.predecessor).fold(arbitrageSequence) { sc =>
              sc +: arbitrageSequence
            }
            if (arbitrageSequence.head == source && arbitrageSequence.last != source) arbitrageSequence = arbitrageSequence :+ source

            val finalAmount = arbitrageSequence.sliding(2).foldLeft(100d) {
              case (amount, pair) =>
                (for {
                  from <- Some(pair.head)
                  to <- Some(pair.last)
                  edgeBetween <- edges.find(edge => edge.from == from && edge.to == to)
                } yield {
                  amount * pow(E, -edgeBetween.negativeLogRate.value)
                }).getOrElse(amount)
            }
            if (finalAmount > 100d) acc :+ Arbitrage(arbitrageSequence, finalAmount) else acc
          } else {
            acc
          }
        }).getOrElse(acc)
    }
  }

  private def continueFindPredecessor(distanceMap: VertexDistanceMap, currentVertex: Currency, arbitrageSequence: Seq[Currency]): Boolean =
    (for {
      vertexDistance <- distanceMap.get(currentVertex)
      predecessor <- vertexDistance.predecessor
    } yield {
      predecessor match {
        case currency if !arbitrageSequence.contains(currency) && currency != source => true
        case _ => false
      }
    }).getOrElse(false)

  private def printReport(arbitrages: Arbitrages): Task[Unit] =
    Task {
      println("Printing report =========================>")
      if (arbitrages.isEmpty) println("No Arbitrage Opportunity.")

      arbitrages.foreach { arbitrage =>
        println("Arbitrage Opportunity:")
        println(arbitrage.sequence.mkString(" -> "))
        println(f"Start with 100 ${arbitrage.sequence.head.value}, end up with ${arbitrage.finalAmount}%.7f ${arbitrage.sequence.head.value} \n")
      }
    }
}

Main.run(List()).runSyncUnsafe()