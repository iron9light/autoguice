package iron9light.autoguice.plugin

import tools.nsc.plugins.PluginComponent
import tools.nsc.transform.Transform
import tools.nsc.ast.TreeDSL
import tools.nsc.symtab.Flags

/**
 * @author il
 */

class GenerateInjectClass(plugin: AutoGuicePlugin) extends PluginComponent with PluginComponentCommon with Transform with TreeDSL {
  val global = plugin.global
  import global._

  val phaseName = this.getClass.getSimpleName
  val runsAfter = "parser" :: Nil

  protected def newTransformer(unit: CompilationUnit): Transformer = GenerateInjectClassTransformer

  case class InjectDefInfo(name: TermName, tpe: Tree, pos: Position = NoPosition)

  object GenerateInjectClassTransformer extends Transformer {
    val injectDefPF: PartialFunction[Tree, InjectDefInfo] = {
      case dd @ DefDef(mods, name, Nil, Nil, tpt, rhs) if rhs.isEmpty =>
        InjectDefInfo(name, tpt, dd.pos.focus)
      case vd @ ValDef(mods, name, tpt, rhs) if rhs.isEmpty =>
        InjectDefInfo(name, tpt, vd.pos.focus)
    }

    def isAutoInject(cd: ClassDef): Boolean = {
      cd.mods.annotations.exists(typer.namer.isAnn(_, "autoinject")) // todo: replace name string with constant value
    }

    def makeClassImpl(oldName: Name, injectDefs: List[InjectDefInfo], pos: Position = NoPosition) = {
      import CODE._
      val name = oldName.append(implSubfix).toTypeName
      val vdMods = Modifiers(Flags.PARAMACCESSOR)
      val vds = injectDefs.map {
        info => {
          ValDef(vdMods, info.name, info.tpe, EmptyTree).setPos(info.pos)
        }
      }

      val mods = Modifiers(Flags.PRIVATE | Flags.SYNTHETIC)
      val parent = Ident(oldName) :: TypeTree(definitions.ScalaObjectClass.tpe) :: Nil
      val self = emptyValDef
//      val injectAnnotation = TypeTree(definitions.getClass("javax.inject.Inject").tpe)
//      val injectAnnotation = NEW(Ident("javax") DOT "inject" DOT "Inject") APPLY()
//      val injectAnnotation = Apply(
//        Select(
//          New(
//            Select(
//              Select(
//                Ident("javax"),
//                "inject"
//              ),
//              "Inject"
//            )
//          ),
//          "<init>"
//        ),
//        Nil
//      )
//      val constrMods = NoMods.withAnnotations(injectAnnotation :: Nil)
      val constrMods = NoMods
      val template = Template(parent, self, constrMods, List(vds), List(Nil), Nil, NoPosition)

      ClassDef(mods, name, Nil, template)
    }

    override def transform(tree: Tree): Tree = {
      val newTree = tree match {
        case packageDef @ PackageDef(pid, stats) =>
          val newStats = stats.flatMap{
            case classDef @ ClassDef(mods, name, tparams, impl) if isAutoInject(classDef) =>
              val classImpl = makeClassImpl(name, impl.body.collect(injectDefPF), classDef.pos.focus)
              classDef :: classImpl :: Nil
            case x => x :: Nil
          }
          treeCopy.PackageDef(packageDef, pid, newStats)
        case _ => tree
      }
      super.transform(newTree)
    }
  }
}