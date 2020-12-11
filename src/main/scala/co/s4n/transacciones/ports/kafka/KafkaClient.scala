package co.s4n.transacciones.ports.kafka

import co.s4n.transacciones.domain.transacciones.modelos.{Mensaje, MensajeConTransaccion, OtroMensaje}
import monix.eval.Task
import monix.kafka._
import monix.execution.Scheduler
import org.apache.kafka.clients.producer.RecordMetadata

object KafkaClient {

  implicit val scheduler: Scheduler = monix.execution.Scheduler.global

  val producerCfg = KafkaProducerConfig.default.copy(
    bootstrapServers = List("127.0.0.1:9092")
  )

  lazy val producer = KafkaProducer[String,String](producerCfg, scheduler)

  def load(mensaje: Mensaje): Task[Option[RecordMetadata]] =
    producer.send(
      "notificaciones",
      mensaje.id,
      mensaje match {
        case MensajeConTransaccion(id, transaccion) =>
          s"${id}," +
            s"${transaccion.tipo}," +
            s"${transaccion.valor.toString()}," +
            s"${transaccion.descripcion}," +
            s"${transaccion.producto.getOrElse("")}," +
            s"${transaccion.fecha.getOrElse("")}"
        case OtroMensaje(id) => s"${id},otro tipo de mensaje,,,,"
      }
    )



  implicit class Redacted(s: String) {
    def redacted(): String = s.replaceAll("\\d", "1")
    def limited(): String = redacted().take(5)
  }

}
