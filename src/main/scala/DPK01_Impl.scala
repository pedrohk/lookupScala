object DPK01_Impl {
  val idToName = Map(1 -> "John")
  val nameToEmail = Map("John" -> "john@john.jhon.com")
  val emailToName = Map("john@john.jhon.com" -> "John")

  def lookup(key: Any): Option[String] = key match
    case id: Int => idToName.get(id)
    case name: String if nameToEmail.contains(name) => nameToEmail.get(name)
    case email: String => emailToName.get(email)
}