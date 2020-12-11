package co.s4n.transacciones.domain

import java.time.{ZoneId, ZonedDateTime}
import co.s4n.transacciones.domain.transacciones.modelos._
import monix.eval.Task
import org.slf4j.LoggerFactory

object Parser {

  val logger = LoggerFactory.getLogger(this.getClass)

  import fastparse._
  import MultiLineWhitespace._

  object TiposTransaccion {
    val todos = List(
      "Bancolombia le informa un Pago de Nomina de",
      "Bancolombia le informa Compra",
      "Bancolombia le informa Transferencia",
      "Bancolombia informa pago Factura Programada",
      "Bancolombia le informa Pago",
      "Bancolombia le informa Retiro por",
      "Bancolombia te informa recepción transferencia",
      "Bancolombia le informa Pago de Tarjeta de Credito por"
    )

    val (
      pagoNomina ::
        compra ::
        transferencia ::
        pagoFactura ::
        pago ::
        retiro ::
        recepcionTranferencia ::
        pagoTarjetaCredito ::
        Nil
      ) = todos
  }

  object OtrosMensajes {
    val todos = List(
      "Bancolombia te informa la Clave Dinámica",
      "el proceso de activaci&oacute;n de la cuenta",
      "bloqueamos su Clave Dinámica",
      "Bancolombia le informa que su factura inscrita",
      "Bancolombia te da la bienvenida al servicio de Clave Dinámica",
      "ha sido desbloqueada",
      "Bancolombia le informa que realizo una inscripcion de cuentas de terceros",
      "cambiaste por SUCURSAL VIRTUAL PERSONAS el medio para recibir la Clave Dinámica",
      "Bancolombia le informa que el cambio de su clave principal ha sido",
      "Bancolombia informa que la solicitud de cambio de topes de transacciones",
      "Bancolombia le informa que ha inscrito la cuenta",
      "Bancolombia le informa Bloqueo Temporal"
    )

    val (
        claveDinamica ::
        activacionDeCuenta ::
        bloqueoClave ::
        facturaInscrita ::
        bienvenidaClaveDinamica ::
        desbloqueo ::
        inscripcionCuentaTerceros ::
        medioRecepcion ::
        cambioClave ::
        cambioTopes ::
        inscripcionCuenta ::
        bloqueoTemporal ::
        Nil
      ) = todos

    // TODO como combinar a partir de la lista?
    def otroMensaje[_ : P]: P[Unit] = P(
      claveDinamica | activacionDeCuenta | bloqueoClave | facturaInscrita | bienvenidaClaveDinamica | desbloqueo |
      inscripcionCuentaTerceros | medioRecepcion |  cambioClave | cambioTopes | inscripcionCuenta | bloqueoTemporal
    )
  }

  val clasificadores = OtrosMensajes.todos ++ TiposTransaccion.todos

  def numero[_ : P]: P[BigDecimal] = P(CharIn("0-9", ",", ".")).rep.!
    .map { s =>
      val hasBothSeparators = s.contains(",") && s.contains(".")
      if (hasBothSeparators && s.matches("^[\\d\\.]+,\\d+$"))
        s.replaceAll("\\.", "").replaceAll(",", ".")
      else if (hasBothSeparators && s.matches("^[\\d,]+\\.\\d+$"))
        s.replaceAll(",", "")
      else s.replaceAll(",", "")
    }
    .map(BigDecimal.apply)

  def palabraAlfanumerica[_ : P]: P[Unit] = {
    import NoWhitespace._

    P(CharIn("A-Z", "a-z", "0-9", "ñÑ", "*.&@", "\\-", "_").rep(1))
  }

  def palabrasAlfanumericas[_ : P]: P[String] = {
    import NoWhitespace._

    P(palabraAlfanumerica ~ " ".rep(1)).rep(1).!.map(_.trim)
  }

  def producto[_ : P]: P[String] = P(!P(". " | ("Inquietudes" ~ AnyChar)) ~ AnyChar).rep.!

  def entero[_ : P]: P[Int] = {
    import NoWhitespace._

    P(CharIn("0-9").rep(1).!.map(_.toInt))
  }

  def horaFecha[_ : P]: P[ZonedDateTime] = {
    import NoWhitespace._

    P(entero.filter(_ <= 23) ~ ":" ~ entero.filter(_ <= 59) ~ CharIn(" ", ".").rep(1) ~ entero ~ "/" ~ entero ~ "/" ~ entero)
      .map {
        case (h, mm, d, m, y) =>
          ZonedDateTime.of(y, m, d, h, mm, 0, 0, ZoneId.of("America/Bogota"))
      }
  }

  def fechaHora[_ : P]: P[ZonedDateTime] = {
    import NoWhitespace._

    P(entero ~ "/" ~ entero ~ "/" ~ entero ~ " " ~ entero.filter(_ <= 23) ~ ":" ~ entero.filter(_ <= 59))
      .map {
        case (d, m, y, h, mm) =>
          ZonedDateTime.of(y, m, d, h, mm, 0, 0, ZoneId.of("America/Bogota"))
      }
  }

  def fechaCorta[_ : P]: P[ZonedDateTime] =
    P(entero ~ "/" ~ entero ~ "/" ~ entero)
      .map { case (d, m, y) => ZonedDateTime.of(y, m, d, 0, 0, 0, 0, ZoneId.of("America/Bogota")) }

  def valorTransaccion[_ : P]: P[BigDecimal] = P("$" ~ numero)

  object Transacciones {
    def compra[_ : P]: P[Transaccion] =
      P(TiposTransaccion.compra ~ "por" ~ valorTransaccion ~ "en" ~ palabrasAlfanumericas ~ horaFecha ~ producto)
        .map(x => Transaccion(
          tipo = Credito,
          descripcion = x._2,
          valor = x._1,
          fecha = Option(x._3),
          producto = Option(x._4)
        ) )

    def transferencia[_ : P]: P[Transaccion] = P(
        TiposTransaccion.transferencia ~ "por" ~ valorTransaccion ~ "desde" ~ producto ~ ".".? ~ fechaHora
      )
      .map(x => Transaccion(
        tipo = Credito,
        descripcion = x._2,
        valor = x._1,
        fecha = Option(x._3),
        producto = Option(x._2)
      ) )

    def recepcionTransferencia[_ : P]: P[Transaccion] = P(
        TiposTransaccion.recepcionTranferencia ~ "de" ~
          palabraAlfanumerica.rep.!.map(_.replaceAll(" por$", "")) ~ valorTransaccion ~
          "en la cuenta *" ~ entero ~ "." ~ fechaHora
      )
      .map(r => Transaccion(
        tipo = Debito,
        descripcion = r._1,
        valor = r._2,
        fecha = Option(r._4),
        producto = Option(r._3.toString)
      ) )

    def retiro[_ : P]: P[Transaccion] = P(
        TiposTransaccion.retiro ~ valorTransaccion ~ "en" ~ palabrasAlfanumericas ~ horaFecha ~ producto
      )
      .map(x => Transaccion(
        tipo = Credito,
        descripcion = x._2.replaceAll("\\. Hora$", ""),
        valor = x._1,
        fecha = Option(x._3),
        producto = Option(x._4)
      ) )

    def pagoTarjeta[_ : P]: P[Transaccion] = P(
        TiposTransaccion.pagoTarjetaCredito ~ valorTransaccion ~ "desde" ~ palabrasAlfanumericas ~ fechaHora
      )
      .map(x =>
        Transaccion(
          tipo = Credito,
          descripcion = x._2,
          valor = x._1,
          producto = Option(x._2),
          fecha = Option(x._3)
      ))

    def pago[_ : P]: P[Transaccion] = P(TiposTransaccion.pago ~ "por" ~ valorTransaccion ~ "a" ~ palabrasAlfanumericas ~ fechaHora)
      .map { x =>
        val descripcion :: producto :: Nil = x._2.split("desde").toList
        Transaccion(
          tipo = Credito,
          descripcion = descripcion.trim,
          valor = x._1,
          fecha = Option(x._3),
          producto = Option(producto.trim)
        )
      }

    def pagoProgramado[_ : P]: P[Transaccion] = P(
        TiposTransaccion.pagoFactura ~ palabraAlfanumerica.rep.!.map(_.replaceAll(" por$", "")) ~
          valorTransaccion ~ "desde" ~ producto ~ ".".? ~ fechaCorta
      )
      .map( x => Transaccion(
        tipo = Credito,
        descripcion = x._1,
        valor = x._2,
        fecha = Option(x._4),
        producto = Option(x._3)
      ) )


    def nomina[_ : P]: P[Transaccion] = P(
        TiposTransaccion.pagoNomina ~ palabraAlfanumerica.rep.!.map(_.replaceAll(" por$", "")) ~ valorTransaccion ~
          "en su Cuenta Ahorros." ~ horaFecha
      )
      .map(x => Transaccion(
        tipo = Debito,
        descripcion = x._1,
        valor = x._2,
        fecha = Option(x._3),
        producto = Option("Cuenta Ahorros")
      ) )

    def transaccion[_ : P]: P[Transaccion] = P(
      compra |
      transferencia |
      recepcionTransferencia |
      retiro |
      pagoTarjeta |
      pago |
      pagoProgramado |
      nomina
    )
  }

  def mensaje[_ : P]: P[Either[Unit, Transaccion]] = P(
    OtrosMensajes.otroMensaje.map(Left.apply) |
    Transacciones.transaccion.map(Right.apply)
  )

  def transform(notificacion: Notificacion): Task[Mensaje] = {
    val m = clasificadores
      .find(notificacion.contenido.contains(_))
      .fold("Could not classify")(clasificador => notificacion.contenido.substring(notificacion.contenido.indexOf(clasificador)))

    parse(m, mensaje(_)).fold(
      (a, b, c) => {
          logger.error("No se pudo procesar notificacion {} {} {}", a, b, c)
          logger.info(notificacion.toString)
          Task.raiseError(new RuntimeException(s"Notificacion ${notificacion.id}, no se pudo procesar."))
        },
      (x, _) => Task.now(x.fold(
        _ => OtroMensaje(notificacion.id),
        transaccion => MensajeConTransaccion(notificacion.id, transaccion)
      ))
    )
  }

}
