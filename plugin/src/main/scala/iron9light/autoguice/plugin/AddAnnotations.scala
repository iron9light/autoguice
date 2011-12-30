package iron9light.autoguice.plugin

import tools.nsc.plugins.PluginComponent
import tools.nsc.transform.Transform

/**
 * @author il
 */

class AddAnnotations(val plugin: AutoGuicePlugin) extends PluginComponent with PluginComponentCommon with Transform {
  val global = plugin.global

  import global._

  val phaseName = this.getClass.getSimpleName

  val runsAfter = "typer" :: Nil

  protected def newTransformer(unit: CompilationUnit): Transformer = new AddImplementedByAnnotationTransformer(unit)

  def implementedByAnnotation(className: String) = {
    val atp: Type = definitions.getClass("com.google.inject.ImplementedBy").tpe
    val implementedClassName = className + implSubfix
    val valueType: Type = definitions.getClass(implementedClassName).tpe
    val annotationInfo = AnnotationInfo(atp, Nil, ("value": Name, LiteralAnnotArg(Constant(valueType))) :: Nil)
    annotationInfo
  }

  val injectAnnotation = {
    val atp = definitions.getClass("javax.inject.Inject").tpe
    AnnotationInfo(atp, Nil, Nil)
  }

  def isInjectClassImpl(symbol: Symbol) = {
    symbol.ancestors.exists(isAutoInjectClass(_))
  }

  class AddImplementedByAnnotationTransformer(unit: CompilationUnit) extends Transformer {
    override def transform(tree: Tree): Tree = {
      val newTree = tree match {
        case classDef @ ClassDef(modifiers, typeName, tparams, impl) if isAutoInjectClass(classDef.symbol) =>
          val symbol = classDef.symbol
          try {
            val annotation = implementedByAnnotation(symbol.fullName)
            symbol.addAnnotation(annotation)
          } catch {
            case e => warning(e.toString)
          }
          classDef
        case classDef: ClassDef if isInjectClassImpl(classDef.symbol) =>
          classDef.impl.body.filter(_.symbol.isPrimaryConstructor).foreach{
            _.symbol.addAnnotation(injectAnnotation)
          }
          classDef
        case _ => tree
      }

      super.transform(newTree)
    }
  }
}