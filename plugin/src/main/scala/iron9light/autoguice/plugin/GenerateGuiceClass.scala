package iron9light.autoguice.plugin

import tools.nsc.plugins.PluginComponent
import tools.nsc.transform.{TypingTransformers, Transform}
import tools.nsc.ast.TreeDSL
import tools.nsc.symtab.Flags

/**
 * @author il
 */

class GenerateGuiceClass(plugin: AutoGuicePlugin) extends PluginComponent with PluginComponentCommon with Transform with TypingTransformers with TreeDSL {
  val global = plugin.global
  import global._

  val phaseName = this.getClass.getSimpleName
  val runsAfter = "typer" :: Nil

  protected def newTransformer(unit: CompilationUnit): Transformer = new GenerateGuiceClassTransformer(unit)

  class GenerateGuiceClassTransformer(unit: CompilationUnit) extends TypingTransformer(unit) {
    def isAbstract(defdef: DefDef) = {
      //      defdef.rhs.isEmpty
      defdef.mods.isDeferred
    }
    
    def addGetters(classDef: ClassDef): ClassDef = {
      val impl = classDef.impl
      val newBody: List[Tree] = impl.body.flatMap {
        tree => tree match {
          case ValDef(mods, name, tpt, rhs) =>
            val sym = tree.symbol
            val zz = sym.getter(sym.owner)
            atOwner(sym.owner) {
              val newName = nme.getterToLocal(name)
              val vdef = treeCopy.ValDef(tree, mods | Flags.PRIVATE | Flags.LOCAL, newName, tpt, rhs)
              vdef.symbol.flags |= (Flags.PRIVATE | Flags.LOCAL)
              vdef.symbol.name = newName
              val getterSym = vdef.symbol.newGetter
              getterSym.flags &= (~Flags.PRIVATE & ~Flags.LOCAL)
              getterSym.info = NullaryMethodType(vdef.symbol.tpe)
              vdef.symbol.owner.info.decls.enter(getterSym)
              val getterDef: DefDef = atPos(vdef.pos.focus) {
                val rhs = gen.mkCheckInit(Select(This(sym.owner), vdef.symbol))
                val r = localTyper.typed {
                  atPos(getterSym.pos.focus) {
                    DefDef(getterSym, rhs)
                  }
                }.asInstanceOf[DefDef]
                r.tpt.setPos(tpt.pos.focus)
                r
              }
              vdef :: getterDef :: Nil
            }
          case _ =>
            tree :: Nil
        }
      }
      val newImpl = treeCopy.Template(impl, impl.parents, impl.self, newBody)
      treeCopy.ClassDef(classDef, classDef.mods, classDef.name, classDef.tparams, newImpl)
    }

    def generateClassImpl(classDef: ClassDef, owner: Symbol): ClassDef = {
      val classSym = classDef.symbol
      val classImplSym: ClassSymbol = owner.newClass(classDef.name.append(implSubfix).toTypeName, classDef.pos.focus)

      classImplSym.flags |= (Flags.PRIVATE | Flags.SYNTHETIC)
      classImplSym.setInfo(
        ClassInfoType(
          definitions.ObjectClass.tpe :: classSym.tpe :: definitions.ScalaObjectClass.tpe :: Nil,
          new Scope(),
          classImplSym
        )
      )
      owner.info.decls.enter(classImplSym)

      val valDefs = for(defdef @ DefDef(_mods, name, _tparams, _vparamss, _tpt, _rhs) <- classDef.impl.body if isAbstract(defdef)) yield {
        val constrParamSym: TermSymbol = classImplSym.newValue(classImplSym.pos.focus, name)
        constrParamSym.setInfo(defdef.symbol.tpe.resultType)
        constrParamSym.setFlag(/*Flags.PRIVATE | Flags.LOCAL |*/ Flags.PARAMACCESSOR)
        classImplSym.info.decls.enter(constrParamSym)

        atOwner(classImplSym) {
          (localTyper.typed{ ValDef(constrParamSym) }).asInstanceOf[ValDef]
        }
      }

      val classImplDef: ClassDef = atOwner(owner) {
        localTyper.typed {
          ClassDef(
            classImplSym,
            NoMods,
            List(valDefs),
            List(List()),
            Nil,
            NoPosition
          )
        }.asInstanceOf[ClassDef]
      }

      classImplDef.impl.body.filter{
        case dd: DefDef => dd.symbol.isPrimaryConstructor
        case _ => false
      }.foreach(
        _.symbol.addAnnotation(AnnotationInfo(definitions.getClass("javax.inject.Inject": Name).tpe, Nil, Nil))
      )

      addGetters(classImplDef)
    }

    override def transform(tree: Tree): Tree = {
      val newTree: Tree = tree match {
        case packageDef @ PackageDef(pid, stats) =>
          val newStats = stats.foldRight(List[Tree]()){
            case (classDef @ ClassDef(modifiers, typeName, tparams, impl), list) if isAutoInjectClass(classDef.symbol) =>
              val classImplDef = generateClassImpl(classDef, packageDef.symbol.moduleClass)
              try {
                val annoTpe: Type = definitions.getClass("com.google.inject.ImplementedBy").tpe
                val annotationInfo = AnnotationInfo(annoTpe, Nil, ("value": Name, LiteralAnnotArg(Constant(classImplDef.symbol.tpe))) :: Nil)
                classDef.symbol.addAnnotation(annotationInfo)
                classDef :: classImplDef :: list
              } catch {
                case e =>
                  warning(e.toString)
                  classDef :: list
              }
            case (t, list) => t :: list
          }

          val newPackageDef = treeCopy.PackageDef(packageDef, pid, newStats)
//          global.treeBrowser.browse(newPackageDef)
          newPackageDef
        case _ => tree
      }
      super.transform(newTree)
    }
  }
}