import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Success

// https://www.youtube.com/watch?v=d-dy1x33moA
object MonadsForBeginners {
	// Monads are types which can take some values and do something interesting with them
	// in a certain structure. The value of a monad lies in its structure.
	case class SafeValue[+T](private val internalValue: T) { // "constructor" = pure, or unit
		def get: T = synchronized {
			// assume it does something interesting
			internalValue
		}

		def transform[S](transformer: T => SafeValue[S]) = synchronized { // bind, or flatMap
			transformer(internalValue)
		}

		// transform is the mondaic equivalent of flatMap: so wee rewrite transform
		def flatMap[S](transformer: T => SafeValue[S]) = synchronized { // bind, or flatMap
			transformer(internalValue)
		}

	}

	// "assume some external API"
	def gimmeSafeValue[T](value: T): SafeValue[T] = SafeValue(value)

	val safeString: SafeValue[String] = gimmeSafeValue("Scala is awesome")
	// extract
	val string = safeString.get
	// transform
	val upperString = string.toUpperCase()
	// wrap
	val upperSafeString = SafeValue(upperString)

	// ETW: Extract Transform and Wrap
	// compressed
	val upperSafeString2 = safeString.transform(s => SafeValue(s.toUpperCase()))

	// Examples
	// Example 1. census
	case class Person(firstName: String, lastName: String) {
		assert(firstName != null && lastName != null)
	}

	// census API
	// bad version of getPerson. Handles null poorly.
	def getPerson(firstName: String, lastName: String): Person =
		if (firstName != null) {
			if (lastName != null) {
				Person(firstName, lastName)
			} else {
				null
			}
		} else {
			null
		}

	// better version of getPerson. Handles null using flatMap.
	def getPersonBetter(firstName: String, lastName: String): Option[Person] =
		Option(firstName).flatMap { fName =>
			Option(lastName).flatMap { lName =>
				Option(Person(fName, lName))
			}
		}

	// best version of getPerson. Handles null using for comprehensions
	def getPersonFor(firstName: String, lastName: String): Option[Person] = for {
		fName <- Option(firstName) // null check is delegated to Option construction
		lName <- Option(lastName) // null check is delegated to Option construction
	} yield Person(fName, lName)

	// Example 2: asynchronous fetches
	case class User(id: String)

	case class Product(sku: String, price: Double)

	def getUser(url: String): Future[User] = Future {
		User("daniel") // sample implementation
	}

	def getLastOrder(userId: String): Future[Product] = Future {
		Product("123-456", 99.99) // sample implementation
	}

	val danielsUrl = "my.store.com/users/daniel"

	// ETW: Extract Transform and Wrap
	getUser(danielsUrl).onComplete({
		case Success(User(id)) =>
			val lastOrder = getLastOrder(id)
			lastOrder.onComplete({
				case Success(Product(sku, p)) =>
					val vatIncludedPrice = p * 1.19
				// pass it on - send Daniel an email
			})
	})
	// a calculate VAT
	val vatInclPrice: Future[Double] = getUser(danielsUrl)
		.flatMap(user => getLastOrder(user.id))
		.map(_.price * 1.19)
	// a calculate VAT
	val vatInclPriceFor: Future[Double] = for {
		user <- getUser(danielsUrl)
		product <- getLastOrder(user.id)
	} yield product.price * 1.19

	// Example 3: double-for loops
	val numbers = List(1, 2, 3)
	val chars = List('a', 'b', 'c')
	// using flatMap
	val checkerboard: List[(Int, Char)] = numbers.flatMap(number => chars.map(char => (number, char)))
	// you can replace nested mappings with for comprehensions
	val checkerboard2 = for {
		number <- numbers
		char <- chars
	} yield (number, char)

	// Properties of Monads

	// prop 1
	def twoConsecutive(x: Int) = List(x, x + 1)

	twoConsecutive(3) // => List(3,4)
	// otherwise you can just do
	List(3).flatMap(twoConsecutive) // => List(3,4)
	// Monad(x).flatMap(f) == f(x)

	// prop 2
	List(1, 2, 3).flatMap(x => List(x)) // => List(1,2,3)
	// in general
	// Monad(v).flatMap(x => Monad(c)) is USELESS, it returns Monad(v)

	// prop 3 - ETW-ETW the ordering of computation doesn't matter to Monads
	val incrementer = (x: Int) => List(x, x + 1)
	val doubler = (x: Int) => List(x, x * 2)
	numbers.flatMap(incrementer).flatMap(doubler)

	// Monad(v).flatMap(f).flatMap(g) == Monad(v).flatMap(x => f(x).flatMap(g))
	def main(args: Array[String]): Unit = {
		println("Hello world!")
		println(numbers.flatMap(incrementer).flatMap(doubler)) // => List(1, 2, 2, 4, 2, 4, 3, 6, 3, 6, 4, 8)
		// the list above is generated as follows
		/*
		 List(
		 incrementer(1).flatMap(doubler) -- 1,2,2,4
		 incrementer(2).flatMap(doubler) -- 2,4,3,6
		 incrementer(3).flatMap(doubler) -- 3,6,4,8
		 )
	 * */
		// this is equivalent to
		// numbers.flatMap(x => incrementer(x).flatMap(doubler))
		println(numbers.flatMap(x => incrementer(x).flatMap(doubler)))
	}
}