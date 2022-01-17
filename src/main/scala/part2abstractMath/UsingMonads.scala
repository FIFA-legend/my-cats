package part2abstractMath

object UsingMonads {

  import cats.Monad
  import cats.instances.list._
  import cats.instances.option._
  val monadList = Monad[List] // fetch the implicit Monad[List]
  val aSimpleList = monadList.pure(2) // List(2)
  val anExtendedList = monadList.flatMap(aSimpleList)(x => List(x, x + 1))
  // applicable to Option, Try, Future

  // either is also a monad
  val aManualEither: Either[String, Int] = Right(42)
  type LoadingOr[T] = Either[String, T]
  type ErrorOr[T] = Either[Throwable, T]

  import cats.instances.either._
  val loadingMonad = Monad[LoadingOr]
  val anEither = loadingMonad.pure(45) // LoadingOr[Int] == Right(45)
  val aChangedLoading = loadingMonad.flatMap(anEither)(n => if (n % 2 == 0) Right(n + 1) else Left("Loading meaning of life..."))

  // imaginary online store
  case class OrderStatus(orderId: Long, status: String)
  def getOrderStatus(orderId: Long): LoadingOr[OrderStatus] = Right(OrderStatus(orderId, "Ready to ship"))
  def trackLocation(orderStatus: OrderStatus): LoadingOr[String] =
    if (orderStatus.orderId > 1000) Left("Not available yet, refreshing data...")
    else Right("Amsterdam, NL")

  val orderId = 457L
  val orderLocation = loadingMonad.flatMap(getOrderStatus(orderId))(orderStatus => trackLocation(orderStatus))

  // use extension methods
  import cats.syntax.flatMap._
  import cats.syntax.functor._
  val orderLocationBetter: LoadingOr[String] = getOrderStatus(orderId).flatMap(trackLocation)
  val orderLocationFor: LoadingOr[String] = for {
    orderStatus <- getOrderStatus(orderId)
    location <- trackLocation(orderStatus)
  } yield location

  // TODO: the service layer API of a web app
  case class Connection(host: String, port: String)
  val config = Map(
    "host" -> "localhost",
    "port" -> "4040"
  )

  trait HttpService[M[_]] {
    def getConnection(cfg: Map[String, String]): M[Connection]
    def issueRequest(connection: Connection, payload: String): M[String]
  }

  def getResponse[M[_] : Monad](service: HttpService[M], payload: String): M[String] = for {
    conn <- service.getConnection(config)
    response <- service.issueRequest(conn, payload)
  } yield response
  // DO NOT CHANGE THE CODE

  /*
    Requirements
    - if the host and port are found in the configuration map, then we'll return a M containing a connection with those values
      otherwise the method will fail, according to the logic of the type M
    - the issueRequest method returns a M containing ths string: "request (payload) has been accepted", if the payload is less than 20 characters
      otherwise the method will fail, according to the logic of the type M

    TODO: provide a real implementation of HttpService using Try, Option, Future, Either
   */

  class OptionHttpService extends HttpService[Option] {
    override def getConnection(cfg: Map[String, String]): Option[Connection] = {
      if (cfg.contains("host") && cfg.contains("port")) Option(Connection(cfg("host"), cfg("port")))
      else None
    }

    override def issueRequest(connection: Connection, payload: String): Option[String] = {
      if (payload.length < 20) Option(s"request ($payload) has been accepted")
      else None
    }
  }

  object OptionHttpService2 extends HttpService[Option] {
    override def getConnection(cfg: Map[String, String]): Option[Connection] = for {
      h <- cfg.get("host")
      p <- cfg.get("port")
    } yield Connection(h, p)

    override def issueRequest(connection: Connection, payload: String): Option[String] = {
      if (payload.length >= 20) None
      else Some(s"request ($payload) has been accepted")
    }
  }


  val responseOption = OptionHttpService2.getConnection(config).flatMap { c =>
    OptionHttpService2.issueRequest(c, "Hello, HttpService")
  }
  val responseOptionFor = for {
    conn <- OptionHttpService2.getConnection(config)
    response <- OptionHttpService2.issueRequest(conn, "Hello, HTTP service")
  } yield response

  // TODO: implement another HttpService with LoadingOr or ErrorOr
  object AggressiveHttpService extends HttpService[ErrorOr] {
    override def getConnection(cfg: Map[String, String]): ErrorOr[Connection] =
      if (!cfg.contains("host") || !cfg.contains("port"))
        Left(new RuntimeException("Connection could not be established: invalid configuration"))
      else
        Right(Connection(cfg("host"), cfg("port")))

    override def issueRequest(connection: Connection, payload: String): ErrorOr[String] =
      if (payload.length >= 20) Left(new RuntimeException("Payload is too large"))
      else Right(s"Request ($payload) was accepted")
  }

  val errorOrResponse: ErrorOr[String] = for {
    conn <- AggressiveHttpService.getConnection(config)
    response <- AggressiveHttpService.issueRequest(conn, "Hello ErrorOr")
  } yield response

  def main(args: Array[String]): Unit = {
    println(responseOption)
    println(errorOrResponse)
    println(getResponse(OptionHttpService2, "Hello, Option"))
    println(getResponse(AggressiveHttpService, "Hello, ErrorOr"))
  }

}
