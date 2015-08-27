package renesca

import akka.actor.ActorSystem
import akka.util.Timeout
import renesca.json.protocols.RequestJsonProtocol._
import renesca.json.protocols.ResponseJsonProtocol._
import spray.client.pipelining._
import spray.http.HttpHeaders.{Authorization, Location, RawHeader}
import spray.http.HttpMethods._
import spray.http.{HttpRequest, _}
import spray.httpx.SprayJsonSupport._
import spray.httpx.unmarshalling._
import spray.json._

import scala.collection.mutable
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

case class TransactionId(id: String) {
  override def toString = id
}

class RestService(val server: String, credentials: Option[BasicHttpCredentials] = None, implicit val timeout: Timeout = Timeout(10.seconds)) {
  // http://spray.io/documentation/1.2.2/spray-can/http-client/request-level/
  // http://spray.io/documentation/1.2.2/spray-client/
  implicit val actorSystem: ActorSystem = ActorSystem()

  // dispatcher provides execution context

  import actorSystem.dispatcher

  val pipeline: HttpRequest => Future[HttpResponse] = sendReceive

  private def awaitResponse(request: HttpRequest): HttpResponse = Await.result(pipeline(request), timeout.duration)

  private def buildHttpPostRequest(path: String, jsonRequest: json.Request): HttpRequest = {
    val headers = new mutable.ListBuffer[HttpHeader]()
    //TODO: Accept: application/json; charset=UTF-8 - is this necessary?
    // val accept:MediaRange = `application/json`// withCharset `UTF-8`
    // headers += Accept(accept)
    headers ++= credentials.map(Authorization(_))
    // accept streaming of results from rest endpoint
    // http://neo4j.com/docs/2.2.3/rest-api-streaming.html
    headers += RawHeader("X-Stream", "true")

    Post(
      uri = Uri(s"$server$path"),
      content = jsonRequest
    ).withHeaders(headers.toList)
  }
def timeColored(text:String, ms:Long):String = {
          val red = "\033[31m"
          val yellow = "\033[33m"
          val reset = "\033[0m"
          if(ms < 100) return text
          else if(ms < 1000) return s"$yellow$text$reset"
          else return s"$red$text$reset"
        }

  private def awaitResponse(path: String, jsonRequest: json.Request): (List[HttpHeader], json.Response) = {
    val httpRequest = buildHttpPostRequest(path, jsonRequest)
    val startTime = System.currentTimeMillis
    val httpResponse = awaitResponse(httpRequest)
    val endTime = System.currentTimeMillis
    val diff = endTime - startTime
    println(timeColored("REQUESTING:\n" + jsonRequest.toJson + "\n-- REQUEST TOOK: " + diff + "ms\n\n", diff))
    val jsonResponse: json.Response = httpResponse.entity.as[json.Response] match {
      case Right(json)                => json
      case Left(deserializationError) => throw new RuntimeException(s"Deserialization Error: $deserializationError\n\n${ httpResponse.entity.asString }")
    }
    //TODO: error handling
    (httpResponse.headers, jsonResponse)
  }

  def singleRequest(jsonRequest: json.Request): json.Response = {
    val path = "/db/data/transaction/commit"
    val (_, jsonResponse) = awaitResponse(path, jsonRequest)
    jsonResponse
  }

  def openTransaction(jsonRequest: json.Request = json.Request()): (TransactionId, json.Response) = {
    val path = "/db/data/transaction"
    val (headers, jsonResponse) = awaitResponse(path, jsonRequest)
    val uris = headers.collectFirst({ case Location(uri) => uri })
    val optionId = for(uri <- uris) yield {
      uri.path.reverse.head.toString
    }

    optionId match {
      case Some(id) => (TransactionId(id), jsonResponse)
      case None     => throw new RuntimeException("Cannot get transaction id")
    }
  }

  def resumeTransaction(id: TransactionId, jsonRequest: json.Request): json.Response = {
    val path = s"/db/data/transaction/$id"
    val (_, jsonResponse) = awaitResponse(path, jsonRequest)
    jsonResponse
  }

  def commitTransaction(id: TransactionId, jsonRequest: json.Request = json.Request()): json.Response = {
    val path = s"/db/data/transaction/$id/commit"
    val (_, jsonResponse) = awaitResponse(path, jsonRequest)
    jsonResponse
  }

  def rollbackTransaction(id: TransactionId) {
    // we don't wait for a response here
    pipeline(HttpRequest(DELETE, s"http://localhost:7474/db/data/transaction/$id"))
  }

  override def toString = s"RestService($server${ if(credentials.isDefined) " with credentials" else "" }, $timeout)"
}

