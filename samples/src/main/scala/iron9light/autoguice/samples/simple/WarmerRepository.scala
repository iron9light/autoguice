package iron9light.autoguice.samples.simple

import com.google.inject.{Guice, Binder, Module}

/**
 * @author il
 * @version 12/26/11 4:19 PM
 */

trait WarmerRepository {
  def createWarmer: Warmer
}

class GuiceWarmerRepository extends WarmerRepository {
  val injector = Guice.createInjector(new MyModule)

  def createWarmer = injector.getInstance(classOf[Warmer])
}

class MyModule extends Module {
  def configure(binder: Binder) {
    import binder._
    bind(classOf[OnOffDevice]) to classOf[Heater]
    bind(classOf[SensorDevice]) to classOf[PotSensor]
    bind(classOf[Warmer]) to classOf[AutoWarmer]
  }
}