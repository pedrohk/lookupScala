import org.scalatest.funsuite.AnyFunSuite

class DPK01Test extends AnyFunSuite {

  test("lookup by id") {
    assert(DPK01_impl_1.lookup(1) == "John")
    assert(DPK01_impl_2.lookup(1) == "John")
    assert(DPK01_impl_3.lookup(1) == "John")
  }

  test("lookup by name") {
    assert(DPK01_impl_1.lookup("John") == "john@john.jhon.com")
    assert(DPK01_impl_2.lookup("John") == "john@john.jhon.com")
    assert(DPK01_impl_3.lookup("John") == "john@john.jhon.com")
  }

  test("lookup by email") {
    assert(DPK01_impl_1.lookup("john@john.jhon.com") == "John")
    assert(DPK01_impl_2.lookup("john@john.jhon.com") == "John")
    assert(DPK01_impl_3.lookup("john@john.jhon.com") == "John")
  }

  test("lookup for non-existing ID") {
    assert(DPK01_impl_1.lookup(99) == "Not found")
    assert(DPK01_impl_2.lookup(99) == "Not found")
    assert(DPK01_impl_3.lookup(99) == "Not found")
  }

  test("lookup for non-existing name") {
    assert(DPK01_impl_1.lookup("NonExisting") == "Not found")
    assert(DPK01_impl_2.lookup("NonExisting") == "Not found")
    assert(DPK01_impl_3.lookup("NonExisting") == "Not found")
  }

  test("lookup for non-existing email") {
    assert(DPK01_impl_1.lookup("nonexisting@domain.com") == "Not found")
    assert(DPK01_impl_2.lookup("nonexisting@domain.com") == "Not found")
    assert(DPK01_impl_3.lookup("nonexisting@domain.com") == "Not found")
  }

  test("lookup for invalid key") {
    assert(DPK01_impl_1.lookup(true) == "Invalid key")
    assert(DPK01_impl_2.lookup(true) == "Invalid key")
    assert(DPK01_impl_3.lookup(true) == "Invalid key")
  }
}
