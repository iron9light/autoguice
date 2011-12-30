package iron9light.autoguice.samples.simple

import org.scalatest.matchers.ShouldMatchers

/**
 * @author il
 */

trait WarmerRepositorySuite extends ShouldMatchers {
  def testWarmerRepository(repository: WarmerRepository) {
    val warmer = repository.createWarmer
    warmer.trigger should be === "heater.on"
  }
}

