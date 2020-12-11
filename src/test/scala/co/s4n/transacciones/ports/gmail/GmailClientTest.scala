package co.s4n.transacciones.ports.gmail

import java.io.File
import java.nio.file.Paths

import com.google.api.services.gmail.model.Message
import org.scalatest.Inside.inside
import org.scalatest.matchers.should
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.propspec.AnyPropSpec

import scala.io.Source
import scala.util.{Success, Try}

class GmailClientTest
  extends AnyPropSpec
  with TableDrivenPropertyChecks
  with should.Matchers {

  property("Decoding messages.") {
    val url = getClass.getResource("/responses")
    val responsesFolder = Paths.get(url.toURI).toFile
    val jsonObjects: Seq[File] = responsesFolder.listFiles().toList.filter(_.isFile)
    val files = Table("file", jsonObjects : _*)

    forAll(files) { file =>
      val json = Source.fromResource(s"responses/${file.getName}").mkString
      val message = GmailClient.jsonFactory.createJsonParser(json).parse(classOf[Message])

      val messageBody = Try {
        new String(message.getPayload.getBody.decodeData())
      } recoverWith {
        case _: NullPointerException => Try {
          new String(message.getPayload.getParts.get(0).getBody.decodeData())
        }
      }

      inside(messageBody) {
        case Success(body) => body should (
          include("Bancolombia le informa un Pago de Nomina") or
          include("Bancolombia le informa Compra") or
          include("Bancolombia le informa Transferencia") or
          include("Bancolombia informa pago Factura Programada") or
//          include("Bancolombia informa pago") or
          include("Bancolombia le informa Pago") or
          include("Bancolombia le informa Retiro por") or
          include("Bancolombia te informa recepción transferencia") or

          include("Bancolombia te informa la Clave Dinámica") or
          include("el proceso de activaci&oacute;n de la cuenta") or
          include("bloqueamos su Clave Dinámica") or
          include("Bancolombia le informa que su factura inscrita") or
          include("Bancolombia te da la bienvenida al servicio de Clave Dinámica") or
          include("ha sido desbloqueada") or
          include("Bancolombia le informa que realizo una inscripcion de cuentas de terceros") or
          include("cambiaste por SUCURSAL VIRTUAL PERSONAS el medio para recibir la Clave Dinámica") or
          include("Bancolombia le informa que el cambio de su clave principal ha sido") or
          include("Bancolombia informa que la solicitud de cambio de topes de transacciones") or
          include("Bancolombia le informa que ha inscrito la cuenta") or
          include("Bancolombia le informa Bloqueo Temporal")
        )
      }
    }
  }

}
