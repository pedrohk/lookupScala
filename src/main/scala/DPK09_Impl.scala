object DPK09_Impl {
  val idLookup: PartialFunction[Any, String] = {
    case 1 => "John"
  }
  val nameLookup: PartialFunction[Any, String] = {
    case "John" => "john@john.jhon.com"
  }
  val emailLookup: PartialFunction[Any, String] = {
    case "john@john.jhon.com" => "John"
  }

  val lookupPF = idLookup.orElse(nameLookup).orElse(emailLookup)

  def lookup(key: Any): Option[String] = if lookupPF.isDefinedAt(key) then Some(lookupPF(key)) else None
}
