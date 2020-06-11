import cats.effect.ExitCode
import cats.implicits._
import monix.eval.{Task, TaskApp}


object test extends TaskApp {
  def run(args: List[String]): Task[ExitCode] = Task.now(println("test")).as(ExitCode.Success)
}

test.run(List())