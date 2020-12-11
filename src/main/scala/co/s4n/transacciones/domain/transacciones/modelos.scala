package co.s4n.transacciones.domain.transacciones

import java.time.ZonedDateTime

package object modelos {

  sealed trait TipoTransaccion
  case object Credito extends TipoTransaccion
  case object Debito extends TipoTransaccion

  case class Transaccion(
    valor: BigDecimal,
    tipo: TipoTransaccion,
    producto: Option[String],
    descripcion: String,
    fecha: Option[ZonedDateTime]
  )

  sealed trait Mensaje { val id: String }
  case class MensajeConTransaccion(id: String, transaccion: Transaccion) extends Mensaje
  case class OtroMensaje(id: String) extends Mensaje

  case class Notificacion(id: String, contenido: String)

}
