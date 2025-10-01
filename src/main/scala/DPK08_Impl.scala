object DPK08_Impl {
  trait LookupStrategy:
    def apply(key: Any): Option[String]

  object IdLookup extends LookupStrategy:
    val map = Map(1 -> "John")

    def apply(key: Any) = key match
      case i: Int => map.get(i)
      case _ => None

  object NameLookup extends LookupStrategy:
    val map = Map("John" -> "john@john.jhon.com")

    def apply(key: Any) = map.get(key.toString)

  object EmailLookup extends LookupStrategy:
    val map = Map("john@john.jhon.com" -> "John")

    def apply(key: Any) = map.get(key.toString)

  val strategies = List(IdLookup, NameLookup, EmailLookup)

  def lookup(key: Any): Option[String] = strategies.view.flatMap(_.apply(key)).headOption
}
