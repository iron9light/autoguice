package iron9light.autoguice.plugin

import org.scalatest.FunSuite
import tools.nsc.{Settings, Global}
import tools.nsc.reporters.ConsoleReporter
import tools.nsc.io.VirtualDirectory
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * @author il
 */

@RunWith(classOf[JUnitRunner])
class AutoGuicePluginSuite extends FunSuite {
  test("") {
    val settings = new Settings
    settings.outputDirs.setSingleOutput(new VirtualDirectory("(memory)", None))
//    settings.browse.value = "typer" :: Nil
//    settings.Ytyperdebug.value = true

    val compiler = new Global(settings, new ConsoleReporter(settings)) {
      override protected def computeInternalPhases() {
        super.computeInternalPhases()
        phasesSet ++= new AutoGuicePlugin(this).components
      }
    }

    new compiler.Run() compile ("src/test/projects/simple/X.scala" :: Nil)

    println("ok")
  }
}