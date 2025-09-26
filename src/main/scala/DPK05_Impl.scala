object DPK05_Impl {
  val data = List(
    (1, "John", "john@john.jhon.com")
  )

  def lookup(key: Any): Option[String] = key match
    case i: Int => data.find(_._1 == i).map(_._2)
    case name: String => data.find(_._2 == name).map(_._3).orElse(data.find(_._3 == name).map(_._2))
}
