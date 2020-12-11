package co.s4n.transacciones.ports.gmail

import co.s4n.transacciones.domain.transacciones.modelos.Notificacion
import com.google.api.client.auth.oauth2.Credential
import com.google.api.client.extensions.java6.auth.oauth2.AuthorizationCodeInstalledApp
import com.google.api.client.extensions.jetty.auth.oauth2.LocalServerReceiver
import com.google.api.client.googleapis.auth.oauth2.{GoogleAuthorizationCodeFlow, GoogleClientSecrets}
import com.google.api.client.googleapis.javanet.GoogleNetHttpTransport
import com.google.api.client.http.javanet.NetHttpTransport
import com.google.api.client.json.jackson2.JacksonFactory
import com.google.api.client.util.store.FileDataStoreFactory
import com.google.api.services.gmail.model.{ListMessagesResponse, Message}
import com.google.api.services.gmail.{Gmail, GmailScopes}
import monix.eval.Task
import monix.execution.Ack
import monix.reactive.observers.Subscriber
import monix.reactive.{Observable, OverflowStrategy}
import org.slf4j.LoggerFactory

import java.io.{File, FileNotFoundException, InputStreamReader}
import scala.jdk.CollectionConverters._
import scala.util.Try
import GmailClient.logger

class GmailClient(service: Gmail) {

  def transacciones(): Observable[Notificacion] = {
    val messagePages: Observable[ListMessagesResponse] =
      Observable.fromTask(fetchMessages()).flatMap { response =>
        Observable.create(OverflowStrategy.Unbounded) { sub =>
          producerLoop(sub, response).runToFuture(sub.scheduler)
        }
      }

    messagePages
      .flatMap(response => Observable.fromIterable(response.getMessages.asScala))
      .flatMap(m => Observable.fromTask(fetchFullMessage(m)))
      .mapEval(bodyToNotif)
  }

  private def bodyToNotif(message: Message): Task[Notificacion] = {
    val messageBody = Try {
      new String(message.getPayload.getBody.decodeData())
    } recoverWith {
      case _: NullPointerException => Try {
        new String(message.getPayload.getParts.get(0).getBody.decodeData())
      }
    }

    Task.fromTry(messageBody)
      .map(messageBody => Notificacion(message.getId, messageBody))
  }

  private def producerLoop(sub: Subscriber[ListMessagesResponse], response: ListMessagesResponse): Task[Unit] = {
    Task.deferFuture {
        logger.info("onNext response {}", response)
        sub.onNext(response)
      }
      .flatMap {
        case Ack.Continue => Option(response.getNextPageToken)
          .fold(Task.unit)(pageToken => fetchMessages(Option(pageToken)).flatMap(response => producerLoop(sub, response)))
        case Ack.Stop => Task.unit
      }
  }

  private def fetchMessages(page: Option[String] = None): Task[ListMessagesResponse] = Task {
    val query = service.users().messages().list("me")
      .setQ("from:alertasynotificaciones@bancolombia.com.co")
      .setIncludeSpamTrash(true)

    page.fold(query.execute())(query.setPageToken(_).execute())
  }

  private def fetchFullMessage(message: Message): Task[Message] = Task {
    service.users().messages().get("me", message.getId).execute()
  }

}



object GmailClient {

  lazy val logger = LoggerFactory.getLogger(getClass)

  private[gmail] lazy val jsonFactory = JacksonFactory.getDefaultInstance

  lazy val gmailService = {
    lazy val httpTransport = GoogleNetHttpTransport.newTrustedTransport

    new Gmail.Builder(
      httpTransport,
      jsonFactory,
      getCredentials(httpTransport, jsonFactory)
    )
    .setApplicationName("APPLICATION_NAME")
    .build();
  }

  private val appScopes = java.util.Arrays.asList(
    GmailScopes.GMAIL_LABELS,
    GmailScopes.GMAIL_MODIFY,
    GmailScopes.GMAIL_READONLY
  )

  private def getCredentials(transport: NetHttpTransport, jacksonFactory: JacksonFactory): Credential = {
    val in = classOf[GmailClient].getResourceAsStream("/credentials.json")
    if (in == null) throw new FileNotFoundException("Resource not found: /credentials.json")
    val clientSecrets = GoogleClientSecrets.load(jacksonFactory, new InputStreamReader(in))

    val flow = new GoogleAuthorizationCodeFlow.Builder(transport, jacksonFactory, clientSecrets, appScopes)
      .setDataStoreFactory(new FileDataStoreFactory(new File("tokens"))).setAccessType("offline").build
    val receiver = new LocalServerReceiver.Builder().setPort(8888).build

    new AuthorizationCodeInstalledApp(flow, receiver).authorize("user")
  }
}
