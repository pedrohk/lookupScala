object DPK02_Impl {
  
  val data = Map(
    "1" -> "John",
    "John" -> "john@john.jhon.com",
    "john@john.jhon.com" -> "John"
  )
  def lookup(key: Any): Option[String] = data.get(key.toString)  
}
