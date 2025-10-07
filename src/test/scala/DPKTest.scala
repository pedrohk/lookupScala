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
}
