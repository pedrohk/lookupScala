object DPK01_impl_1 {
  val users = Map(
    1 -> ("John", "john@john.jhon.com"),
    2 -> ("Jane", "jane@jane.jane.com")
  )
  val nameToEmail = users.map { case (_, (n, e)) => n -> e }
  val emailToName = users.map { case (_, (n, e)) => e -> n }
  def lookup(key: Any): String = key match {
    case id: Int => users.get(id).map(_._1).getOrElse("Not found")
    case s: String => nameToEmail.get(s).orElse(emailToName.get(s)).getOrElse("Not found")
    case _ => "Invalid key"
  }
}
