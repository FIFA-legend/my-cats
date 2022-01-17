package part2abstractMath

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}

object Monads {

  // lists
  val numbersList = List(1, 2, 3)
  val charsList = List('a', 'b', 'c')

  // TODO 1.1: how would you create all combinations of (number, char)?
  val combinationsList: List[(Int, Char)] = numbersList.flatMap(n => charsList.map(c => (n, c)))
  val combinationsListFor = for {
    n <- numbersList
    c <- charsList
  } yield (n, c) // identical

  // options
  val numberOption = Option(2)
  val charOption = Option('d')
  // TODO 1.2: how would you create all combinations of (number, char)?
  val combinationOption = numberOption.flatMap(n => charOption.map(c => (n, c)))
  val combinationOptionFor = for {
    n <- numberOption
    c <- charOption
  } yield (n, c)

  // futures
  implicit val ex: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))
  val numberFuture = Future(42)
  val charFuture = Future('z')
  // TODO 1.3: how would you create all combinations of (number, char)?
  val combinationFuture = numberFuture.flatMap(n => charFuture.map(c => (n, c)))
  val combinationFutureFor = for {
    n <- numberFuture
    c <- charFuture
  } yield (n, c)

  /*
    Pattern
    - wrapping a value into a monadic value
    - the flatMap mechanism

    MONADS
   */
  trait MyMonad[M[_]] {
    def pure[A](value: A): M[A]
    def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]
    // TODO: implement this
    def map[A, B](ma: M[A])(f: A => B): M[B] = flatMap(ma)(f.andThen(pure)) // my variant
    def map2[A, B](ma: M[A])(f: A => B): M[B] = flatMap(ma)(x => pure(f(x))) // rockthejvm variant
  }

  // Cats Monad
  import cats.Monad
  import cats.instances.option._ // implicit Monad[Option]
  val optionMonad = Monad[Option]
  val anOption = optionMonad.pure(4) // Option(4) == Some(4)
  val aTransformedOption = optionMonad.flatMap(anOption)(x => if (x % 3 == 0) Some(x + 1) else None)

  import cats.instances.list._
  val listMonad = Monad[List]
  val aList = listMonad.pure(3) // List(3)
  val aTransformedList = listMonad.flatMap(aList)(x => List(x, x + 1)) // List(3, 4)

  // TODO 2: use a Monad[Future]
  import cats.instances.future._
  val futureMonad = Monad[Future]
  val aFuture = futureMonad.pure(43) // requires an implicit ExecutionContext
  val aTransformedFuture = futureMonad.flatMap(aFuture)(x => Future(x + 44)) // future that will end up with a Success(87)

  // specialized API
  def getPairsList(numbers: List[Int], chars: List[Char]): List[(Int, Char)] = numbers.flatMap(n => chars.map(c => (n, c)))
  def getPairsOption(number: Option[Int], char: Option[Char]): Option[(Int, Char)] = number.flatMap(n => char.map(c => (n, c)))
  def getPairsFuture(number: Future[Int], char: Future[Char]): Future[(Int, Char)] = number.flatMap(n => char.map(c => (n, c)))

  // generalize
  def getPairs[M[_], A, B](ma: M[A], mb: M[B])(implicit monad: Monad[M]): M[(A, B)] =
    monad.flatMap(ma)(a => monad.map(mb)(b => (a, b)))

  // extension methods - weirder imports - pure, flatMap
  import cats.syntax.applicative._ // pure is here
  val oneOption = 1.pure[Option] // implicit Monad[Option] will be used => Some(1)
  val oneList = 1.pure[List] // List(1)

  import cats.syntax.flatMap._ // flatMap is here
  val oneOptionTransformed = oneOption.flatMap(x => (x + 1).pure[Option])

  // TODO 3: implement the map method in MyMonad
  // Monad extends Functors
  val oneOptionMapped2 = Monad[Option].map(Option(2))(_ + 1)
  import cats.syntax.functor._ // map is here
  val oneOptionMapped = oneOption.map(_ + 1)

  // for-comprehensions
  val composedOption = for {
    one <- 1.pure[Option]
    two <- 2.pure[Option]
  } yield one + two

  // TODO 4: implement a shorter version of getPairs using for-comprehension
  def getPairs2[M[_] : Monad, A, B](ma: M[A], mb: M[B]): M[(A, B)] = for {
    a <- ma
    b <- mb
  } yield (a, b)

  def main(args: Array[String]): Unit = {
    println(getPairs2(numbersList, charsList))
    println(getPairs2(numberOption, charOption))
    getPairs2(numberFuture, charFuture).foreach(println)
  }

}
