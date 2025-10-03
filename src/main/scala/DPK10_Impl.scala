object DPK10_Impl {
  case class Record(id: Int, name: String, email: String)

  val records = List(Record(1, "John", "john@john.jhon.com"))

  def lookup(key: Any): Option[String] =
    records.collectFirst {
      case r if r.id == key => r.name
      case r if r.name == key => r.email
      case r if r.email == key => r.name
    }
}
