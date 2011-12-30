package iron9light.autoguice.samples.simple

import iron9light.autoguice.annotation.autoinject

/**
 * @author il
 */

trait OnOffDevice {
  def on: String

  def off: String
}

class Heater extends OnOffDevice {
  def on: String = {
    println("heater.on")
    "heater.on"
  }

  def off: String = {
    println("heater.off")
    "heater.off"
  }
}

trait SensorDevice {
  def isCoffeePresent: Boolean
}

class PotSensor extends SensorDevice {
  def isCoffeePresent = true
}

trait Warmer {
  def trigger: String
}

@autoinject
//@com.google.inject.ImplementedBy(classOf[AutoWarmerImpl])
trait AutoWarmer extends Warmer {
  def onOff: OnOffDevice

  def sensor: SensorDevice

  def trigger = {
    if (sensor.isCoffeePresent) onOff.on
    else onOff.off
  }
}

private class AutoWarmerImpl @javax.inject.Inject()(val onOff: OnOffDevice, val sensor: SensorDevice) extends AutoWarmer