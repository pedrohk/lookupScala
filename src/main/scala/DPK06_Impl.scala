object DPK06_Impl {
  val idToName = Map(1 -> "John")
  val nameToEmail = Map("John" -> "john@john.jhon.com")
  val emailToName = nameToEmail.map(_.swap)

  def lookup(key: Any): Option[String] = key match
    case i: Int => idToName.get(i)
    case s: String => nameToEmail.get(s).orElse(emailToName.get(s))
}
