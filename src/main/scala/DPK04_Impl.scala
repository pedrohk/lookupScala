class DPK04_Impl {
  val idToName = Map(1 -> "John")
  val nameToEmail = Map("John" -> "john@john.jhon.com")
  val emailToName = Map("john@john.jhon.com" -> "John")

  val lookupFuncs: List[Any => Option[String]] = List(
    { case i: Int => idToName.get(i) },
    { case s: String if nameToEmail.contains(s) => nameToEmail.get(s) },
    { case s: String => emailToName.get(s) }
  )

  def lookup(key: Any): Option[String] = lookupFuncs.view.flatMap(f => f(key)).headOption
}
