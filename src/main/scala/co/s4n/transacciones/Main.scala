package co.s4n.transacciones

import cats.effect.ExitCase
import cats.effect.ExitCase.Completed
import co.s4n.transacciones.domain.Parser
import co.s4n.transacciones.ports.gmail.GmailClient
import co.s4n.transacciones.ports.kafka.KafkaClient
import monix.eval.Task
import monix.execution.Scheduler
import org.slf4j.LoggerFactory

object Main extends App {

  lazy val logger = LoggerFactory.getLogger(getClass)
  implicit val scheduler = Scheduler.fixedPool(name = "main", poolSize = 1, daemonic = false)

  new GmailClient(GmailClient.gmailService)
    .transacciones()
    .mapEval(Parser.transform)
    .mapEval(KafkaClient.load)

    .guaranteeCase {
      case Completed => Task.now(logger.info("Processing complete"))
      case e: ExitCase.Error[Throwable] => Task.now(logger.error("Finished with error", e))
    }
    .foreach(_ => logger.info("Processing messages."))

}
