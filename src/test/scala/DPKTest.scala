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

  test("5. Using Tuple Matching") {
    val data = List((1, "John", "john@john.jhon.com"))

    def lookup(key: Any): Option[String] = key match {
      case i: Int => data.find(_._1 == i).map(_._2)
      case name: String => data.find(_._2 == name).map(_._3).orElse(data.find(_._3 == name).map(_._2))
    }

    lookup(1) shouldBe Some("John")
    lookup("John") shouldBe Some("john@john.jhon.com")
    lookup("john@john.jhon.com") shouldBe Some("John")
    lookup("other") shouldBe None
  }

  test("6. BiMap-Like Simulation") {
    val idToName = Map(1 -> "John")
    val nameToEmail = Map("John" -> "john@john.jhon.com")
    val emailToName = nameToEmail.map(_.swap)

    def lookup(key: Any): Option[String] = key match {
      case i: Int => idToName.get(i)
      case s: String => nameToEmail.get(s).orElse(emailToName.get(s))
    }

    lookup(1) shouldBe Some("John")
    lookup("John") shouldBe Some("john@john.jhon.com")
    lookup("john@john.jhon.com") shouldBe Some("John")
    lookup("none") shouldBe None
  }

  test("7. Trie-Based Prefix Map Lookup") {
    val data = Map(
      "id:1" -> "John",
      "name:John" -> "john@john.jhon.com",
      "email:john@john.jhon.com" -> "John"
    )

    def lookup(key: Any): Option[String] = key match {
      case i: Int => data.get(s"id:$i")
      case s: String => data.get(s"name:$s").orElse(data.get(s"email:$s"))
    }

    lookup(1) shouldBe Some("John")
    lookup("John") shouldBe Some("john@john.jhon.com")
    lookup("john@john.jhon.com") shouldBe Some("John")
    lookup("x") shouldBe None
  }

  test("8. Strategy Pattern") {
    trait LookupStrategy:
      def apply(key: Any): Option[String]

    object IdLookup extends LookupStrategy {
      val map = Map(1 -> "John")

      def apply(key: Any): Option[String] = key match {
        case i: Int => map.get(i)
        case _ => None
      }
    }

    object NameLookup extends LookupStrategy {
      val map = Map("John" -> "john@john.jhon.com")

      def apply(key: Any): Option[String] = map.get(key.toString)
    }

    object EmailLookup extends LookupStrategy {
      val map = Map("john@john.jhon.com" -> "John")

      def apply(key: Any): Option[String] = map.get(key.toString)
    }

    val strategies = List(IdLookup, NameLookup, EmailLookup)

    def lookup(key: Any): Option[String] = strategies.view.flatMap(_.apply(key)).headOption

    lookup(1) shouldBe Some("John")
    lookup("John") shouldBe Some("john@john.jhon.com")
    lookup("john@john.jhon.com") shouldBe Some("John")
    lookup("none") shouldBe None
  }

  test("9. PartialFunction Chain") {
    val idLookup: PartialFunction[Any, String] = {
      case 1 => "John"
    }
    val nameLookup: PartialFunction[Any, String] = {
      case "John" => "john@john.jhon.com"
    }
    val emailLookup: PartialFunction[Any, String] = {
      case "john@john.jhon.com" => "John"
    }

    val lookupPF = idLookup.orElse(nameLookup).orElse(emailLookup)

    def lookup(key: Any): Option[String] = if (lookupPF.isDefinedAt(key)) Some(lookupPF(key)) else None

    lookup(1) shouldBe Some("John")
    lookup("John") shouldBe Some("john@john.jhon.com")
    lookup("john@john.jhon.com") shouldBe Some("John")
    lookup("invalid") shouldBe None
  }

  test("10. Index-Based Filtering") {
    case class Record(id: Int, name: String, email: String)
    val records = List(Record(1, "John", "john@john.jhon.com"))

    def lookup(key: Any): Option[String] =
      records.collectFirst {
        case r if r.id == key => r.name
        case r if r.name == key => r.email
        case r if r.email == key => r.name
      }

    lookup(1) shouldBe Some("John")
    lookup("John") shouldBe Some("john@john.jhon.com")
    lookup("john@john.jhon.com") shouldBe Some("John")
    lookup("none") shouldBe None
  }

}
