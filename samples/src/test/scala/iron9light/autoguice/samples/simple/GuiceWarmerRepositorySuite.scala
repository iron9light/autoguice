package iron9light.autoguice.samples.simple

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * @author il
 */

@RunWith(classOf[JUnitRunner])
class GuiceWarmerRepositorySuite extends WarmerRepositorySuite with FunSuite {
  test("") {
    val repository = new GuiceWarmerRepository
    testWarmerRepository(repository)
  }
}