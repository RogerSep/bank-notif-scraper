package co.s4n.transacciones.domain

import java.io.File
import java.nio.file.Paths
import java.time.{ZoneId, ZonedDateTime}
import co.s4n.transacciones.domain.transacciones.modelos.{Credito, Debito, TipoTransaccion, Transaccion}
import org.scalatest.matchers._
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import scala.io.Source

class ParserTest extends AnyPropSpec
  with ScalaCheckDrivenPropertyChecks
  with TableDrivenPropertyChecks
  with should.Matchers {

  import Parser._
  import fastparse._

  val numerosFormateados = Table(
    ("Valores con formato", "Valor esperado"),
    ("4,181,300.00", BigDecimal(4181300)),
    ("4181300", BigDecimal(4181300)),
    ("10,000.00", BigDecimal(10000)),
    ("10000", BigDecimal(10000)),
    ("5,899,234.001234", BigDecimal(5899234.001234)),
    ("12.499,00", BigDecimal(12499)),
    ("27,000", BigDecimal(27000))
  )

  property("Conversion de numero formateado") {
    forAll(numerosFormateados) { (valorConFormato: String, valorEsperado: BigDecimal) =>
      val result = parse(valorConFormato, numero(_))
      result shouldBe Parsed.Success(valorEsperado, valorConFormato.length)
    }
  }

  property("Conversión de valor monetario") {
    forAll(numerosFormateados) { (valorConFormato: String, valorEsperado: BigDecimal) =>
      val result = parse(s"$$$valorConFormato", valorTransaccion(_))
      result shouldBe Parsed.Success(valorEsperado, valorConFormato.length + 1)
    }
  }

  property("Palabras alfanumericas") {
    val ejemplos = Table(
      ("Palabras", "Resultado esperado"),
      ("hola hello hi -break", "hola hello hi"),
      ("hola 12:21", "hola"),
      ("CARULLA PINAR DEL RI 13:56", "CARULLA PINAR DEL RI"),
      ("COLS S A  CLL 07:56.", "COLS S A  CLL")
    )

    forAll(ejemplos) { (ejemplo, resultadoEsperado) =>
      val Parsed.Success(resultado, _) = parse(ejemplo, palabrasAlfanumericas(_))

      resultado shouldBe resultadoEsperado
    }
  }

  property("Parses fechas") {
    val ejemplos = Table(
      ("Fecha", "Resultado"),
      ("13:42. 02/08/2018", ZonedDateTime.of(2018, 8, 2, 13, 42, 0, 0, ZoneId.of("America/Bogota"))),
      ("17:54. 10/08/2020", ZonedDateTime.of(2020, 8, 10, 17, 54, 0, 0, ZoneId.of("America/Bogota"))),
      ("17:54 10/08/2020", ZonedDateTime.of(2020, 8, 10, 17, 54, 0, 0, ZoneId.of("America/Bogota")))
    )

    forAll(ejemplos) { (ejemplo, resultadoEsperado) =>
      parse(ejemplo, horaFecha(_)) shouldBe Parsed.Success(resultadoEsperado, ejemplo.length)
    }
  }

  property("Detectar otro tipo de mensaje") {
    val ejemplos = Table("Otro mensaje", OtrosMensajes.todos: _*)

    forAll(ejemplos) { ejemplo =>
      parse(ejemplo, OtrosMensajes.otroMensaje(_)) shouldBe Parsed.Success((), ejemplo.length)
    }
  }

  property("Detectar compras") {
    val ejemplos = Table(
      ("Ejemplo", "Resultado esperado"),
      (
        "Bancolombia le informa Compra por $41.409,00 en INVERSIONES SACE 13:42. 02/08/2018 T.Deb *7865",
        (Credito, "INVERSIONES SACE", BigDecimal(41409), "2018-08-02T13:42:00", "T.Deb *7865")
      ),
      (
        "Bancolombia le informa Compra por $165.500,00 en HOMECENTER 17:54. 10/08/2020 T.Cred *2894",
        (Credito, "HOMECENTER", BigDecimal(165500), "2020-08-10T17:54:00", "T.Cred *2894")
      ),
      (
        "Bancolombia le informa Compra por $58.780,00 en CARULLA PINAR DEL RI 13:56. 27/09/2017 T.Deb *1243 wefewf Inquietudes blah",
        (Credito, "CARULLA PINAR DEL RI", BigDecimal(58780), "2017-09-27T13:56:00", "T.Deb *1243 wefewf")
      ),
      (
        "Bancolombia le informa Compra por $31.200,00 en COLS S A  CLL 07:56. 28/11/2019 T.Cred *2894. Inquietudes",
        (Credito, "COLS S A  CLL", BigDecimal(31200), "2019-11-28T07:56:00", "T.Cred *2894")
      )
    )

    forAll(ejemplos) { (ejemplo, resultadoEsperado) =>
      val Parsed.Success(resultado, _) = parse(ejemplo, Transacciones.compra(_))
      resultado shouldBe resultadoEsperado.transaccion
    }
  }

  property("Detectar transferencias") {
    val ejemplos = Table(
      ("Ejemplo", "Resultado esperado"),
      (
        "Bancolombia le informa Transferencia por $38,170 desde cta *9082 a cta 93353415709. 06/09/2018 15:35",
        (Credito, "cta *9082 a cta 93353415709", BigDecimal(38170), "2018-09-06T15:35:00", "cta *9082 a cta 93353415709")
      ),
      (
        "Bancolombia le informa Transferencia por $81,400 desde cta *9082 a cta 03004621188. 20/07/2020 21:22",
        (Credito, "cta *9082 a cta 03004621188", BigDecimal(81400), "2020-07-20T21:22:00", "cta *9082 a cta 03004621188")
      )
    )

    forAll(ejemplos) { (ejemplo, resultadoEsperado) =>
      val Parsed.Success(resultado, _) = parse(ejemplo, Transacciones.transferencia(_))
      resultado shouldBe resultadoEsperado.transaccion
    }
  }

  property("Detectar recepcion de transferencias") {
    val ejemplos = Table(
      ("Ejemplo", "Resultado esperado"),
      (
        "Bancolombia te informa recepción transferencia de DIANA BURGOS por $16,500 en la cuenta *9082. 01/03/2020 15:27",
        (Debito, "DIANA BURGOS", BigDecimal(16500), "2020-03-01T15:27:00", "9082")
      ),
      (
        "Bancolombia te informa recepción transferencia de MELINA MEJIA por $32,670 en la cuenta *9082. 21/02/2020 15:02",
        (Debito, "MELINA MEJIA", BigDecimal(32670), "2020-02-21T15:02:00", "9082")
      )
    )

    forAll(ejemplos) { (ejemplo, resultadoEsperado) =>
      val Parsed.Success(resultado, _) = parse(ejemplo, Transacciones.recepcionTransferencia(_))
      resultado shouldBe resultadoEsperado.transaccion
    }
  }

  property("Detectar retiros") {
    val ejemplos = Table(
      ("Ejemplo", "Resultado esperado"),
      (
        "Bancolombia le informa Retiro por $200.000,00 en POBLADOPARQ. Hora 20:42 21/01/2020 T.Deb *7865",
        (Credito, "POBLADOPARQ", BigDecimal(200000), "2020-01-21T20:42:00", "T.Deb *7865")
      ),
      (
        "Bancolombia le informa Retiro por $600.000,00 en MF_SURAMER2. Hora 14:16 20/03/2020 T.Deb *7865. Inquietudes al 0345109095/018000931987.",
        (Credito, "MF_SURAMER2", BigDecimal(600000), "2020-03-20T14:16:00", "T.Deb *7865")
      )
    )

    forAll(ejemplos) { (ejemplo, resultadoEsperado) =>
      val Parsed.Success(resultado, _) = parse(ejemplo, Transacciones.retiro(_))
      resultado shouldBe resultadoEsperado.transaccion
    }
  }

  property("Detectar pagos") {
    val ejemplos = Table(
      ("Ejemplo", "Resultado esperado"),
      (
        "Bancolombia le informa Pago por $25,000.00 a VIRGIN MOBILE COLOMB desde cta *9082. 21/07/2020 21:55",
        (Credito, "VIRGIN MOBILE COLOMB", BigDecimal(25000), "2020-07-21T21:55:00", "cta *9082.")
      ),
      (
        "Bancolombia le informa Pago por $174,729.00 a CLARO - TELMEX HOGAR desde cta *9082. 08/08/2018 18:10. Inquietudes",
        (Credito, "CLARO - TELMEX HOGAR", BigDecimal(174729), "2018-08-08T18:10:00", "cta *9082.")
      )
    )

    forAll(ejemplos) { (ejemplo, resultadoEsperado) =>
      val Parsed.Success(resultado, _) = parse(ejemplo, Transacciones.pago(_))
      resultado shouldBe resultadoEsperado.transaccion
    }
  }

  property("Detectar pagos de tarjeta de crédito") {
    val ejemplos = Table(
      ("Ejemplo", "Resultado esperado"),
      (
        "Bancolombia le informa Pago de Tarjeta de Credito por $1,242,036 desde cta *9082 a la tarjeta *1242. 31/12/2019 12:56",
        (Credito, "cta *9082 a la tarjeta *1242.", BigDecimal(1242036), "2019-12-31T12:56:00", "cta *9082 a la tarjeta *1242.")
      )
    )

    forAll(ejemplos) { (ejemplo, resultadoEsperado) =>
      val Parsed.Success(resultado, _) = parse(ejemplo, Transacciones.pagoTarjeta(_))
      resultado shouldBe resultadoEsperado.transaccion
    }
  }

  property("Detectar pagos de nomina") {
    val ejemplos = Table(
      ("Ejemplo", "Resultado esperado"),
      (
        "Bancolombia le informa un Pago de Nomina de SAN S A S por $1,181,321.00 en su Cuenta Ahorros. 12:08 01/04/2030. Inquietudes al 018000931987.",
        (Debito, "SAN S A S", BigDecimal(1181321), "2030-04-01T12:08:00", "Cuenta Ahorros")
      )
    )

    forAll(ejemplos) { (ejemplo, resultadoEsperado) =>
      val Parsed.Success(resultado, _) = parse(ejemplo, Transacciones.nomina(_))
      resultado shouldBe resultadoEsperado.transaccion
    }
  }

  property("Detectar pago programado") {
    val ejemplos = Table(
      ("Ejemplo", "Resultado esperado"),
      (
        "Bancolombia informa pago Factura Programada CLARO SOLUCIONE  Ref 76245121 por $181.738,00 desde Aho*9082. 15/05/2020.",
        (Credito, "CLARO SOLUCIONE  Ref 76245121", BigDecimal(181738), "2020-05-15T00:00:00", "Aho*9082")
      ),
      (
        "Bancolombia informa pago Factura Programada EPM SERVICIOS P  Ref 1121557101 por $162.714,00 desde Aho*9082. 30/12/2019.",
        (Credito, "EPM SERVICIOS P  Ref 1121557101", BigDecimal(162714), "2019-12-30T00:00:00", "Aho*9082")
      )
    )

    forAll(ejemplos) { (ejemplo, resultadoEsperado) =>
      val Parsed.Success(resultado, _) = parse(ejemplo, Transacciones.pagoProgramado(_))
      resultado shouldBe resultadoEsperado.transaccion
    }
  }

  ignore("Decoding messages.") {
    val url = getClass.getResource("/responses/decoded")
    val responsesFolder = Paths.get(url.toURI).toFile
    val messages: Seq[File] = responsesFolder.listFiles().toList.filter(_.isFile)
    val files = Table("file", messages: _*)

    val clasificadores = OtrosMensajes.todos ++ TiposTransaccion.todos

    forAll(files) { file =>
      val message = Source.fromResource(s"responses/decoded/${file.getName}").mkString
      val m = clasificadores
        .find(message.contains(_))
        .fold("Could not classify")(clasificador => message.substring(message.indexOf(clasificador)))

      try {
        val parsed = parse(m, mensaje(_))

        parsed shouldBe a[Parsed.Success[Either[_, _]]]
      } catch {
        case e => {
          println(m)
          throw e
        }
      }
    }
  }

  implicit class Tran(val t: (TipoTransaccion, String, BigDecimal, String, String)) {
    def transaccion: Transaccion = Transaccion(
      tipo = t._1,
      descripcion = t._2,
      valor = t._3,
      fecha = Option(t._4).filter(_.nonEmpty).map(s => ZonedDateTime.parse(s"${s}-05:00[America/Bogota]")),
      producto = Option(t._5).filter(_.nonEmpty)
    )
  }
}
