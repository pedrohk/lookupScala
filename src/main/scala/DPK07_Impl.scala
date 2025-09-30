object DPK07_Impl {
  val data = Map(
    "id:1" -> "John",
    "name:John" -> "john@john.jhon.com",
    "email:john@john.jhon.com" -> "John"
  )

  def lookup(key: Any): Option[String] = key match
    case i: Int => data.get(s"id:$i")
    case s: String => data.get(s"name:$s").orElse(data.get(s"email:$s"))
}
