package iron9light.autoguice.plugin

import tools.nsc
import nsc.plugins.{PluginComponent, Plugin}
import nsc.Global

/**
 * @author il
 */

class AutoGuicePlugin(val global: Global) extends Plugin {
  val name = "autoguice"
  val description = "support for the @autoinject annotation"

  val components =
    new GenerateGuiceClass(this) ::
      // new GenerateInjectClass(this) ::
      // new AddAnnotations(this) ::
      Nil
}

trait PluginComponentCommon {self: PluginComponent =>
  import self.global._

  val autoInjectAnnotationClass = "iron9light.autoguice.annotation.autoinject"

  def isAutoInjectClass(symbol: Symbol) = {
    try {
      val autoInjectAnnotation = definitions.getClass(autoInjectAnnotationClass: Name)
      if(symbol != null) {
        symbol.hasAnnotation(autoInjectAnnotation)
      } else false
    } catch {
      case _ => false
    }
  }

  val implSubfix = "__Impl"
}