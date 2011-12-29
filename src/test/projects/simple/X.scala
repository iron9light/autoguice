package simple

import iron9light.autoguice.annotation.autoinject

@autoinject
//@com.google.inject.ImplementedBy(classOf[XX])
trait X extends (() => Int) {
  def name: String

  def someValue: Int

  def p = classOf[X]

  def apply() = 5
}

private class XX @javax.inject.Inject()(val name: String, val someValue: Int) extends X