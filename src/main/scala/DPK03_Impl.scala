object DPK03_Impl {
  case class Person(id: Int, name: String, email: String)

  val people = List(Person(1, "John", "john@john.jhon.com"))

  def lookup(key: Any): Option[String] = key match
    case id: Int => people.find(_.id == id).map(_.name)
    case name: String if people.exists(_.name == name) => people.find(_.name == name).map(_.email)
    case email: String => people.find(_.email == email).map(_.name)
}
