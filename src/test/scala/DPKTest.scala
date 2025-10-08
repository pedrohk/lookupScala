import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class DPK01Test extends AnyFunSuite with Matchers {
  test("1. Basic Bidirectional Map") {
    val idToName = Map(1 -> "John")
    val nameToEmail = Map("John" -> "john@john.jhon.com")
    val emailToName = Map("john@john.jhon.com" -> "John")

    def lookup(key: Any): Option[String] = key match {
      case id: Int => idToName.get(id)
      case name: String if nameToEmail.contains(name) => nameToEmail.get(name)
      case email: String => emailToName.get(email)
    }

    lookup(1) shouldBe Some("John")
    lookup("John") shouldBe Some("john@john.jhon.com")
    lookup("john@john.jhon.com") shouldBe Some("John")
    lookup(99) shouldBe None
  }

  test("2. Unified Map Lookup") {
    val data = Map(
      "1" -> "John",
      "John" -> "john@john.jhon.com",
      "john@john.jhon.com" -> "John"
    )

    def lookup(key: Any): Option[String] = data.get(key.toString)

    lookup(1) shouldBe Some("John")
    lookup("John") shouldBe Some("john@john.jhon.com")
    lookup("john@john.jhon.com") shouldBe Some("John")
    lookup("unknown") shouldBe None
  }

  test("3. Pattern Matching with Case Classes") {
    case class Person(id: Int, name: String, email: String)
    val people = List(Person(1, "John", "john@john.jhon.com"))

    def lookup(key: Any): Option[String] = key match {
      case id: Int => people.find(_.id == id).map(_.name)
      case name: String if people.exists(_.name == name) => people.find(_.name == name).map(_.email)
      case email: String => people.find(_.email == email).map(_.name)
    }

    lookup(1) shouldBe Some("John")
    lookup("John") shouldBe Some("john@john.jhon.com")
    lookup("john@john.jhon.com") shouldBe Some("John")
    lookup("invalid") shouldBe None
  }

  test("4. Function Registry") {
    val idToName = Map(1 -> "John")
    val nameToEmail = Map("John" -> "john@john.jhon.com")
    val emailToName = Map("john@john.jhon.com" -> "John")

    val lookupFuncs: List[Any => Option[String]] = List(
      {
        case i: Int => idToName.get(i)
        case _ => None
      },
      {
        case s: String if nameToEmail.contains(s) => nameToEmail.get(s)
        case _ => None
      },
      {
        case s: String => emailToName.get(s)
        case _ => None
      }
    )

    def lookup(key: Any): Option[String] = lookupFuncs.view.flatMap(f => f(key)).headOption

    lookup(1) shouldBe Some("John")
    lookup("John") shouldBe Some("john@john.jhon.com")
    lookup("john@john.jhon.com") shouldBe Some("John")
    lookup("unknown") shouldBe None
  }

}
