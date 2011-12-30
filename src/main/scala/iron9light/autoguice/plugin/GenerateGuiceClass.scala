package iron9light.autoguice.plugin

import tools.nsc.plugins.PluginComponent
import tools.nsc.transform.{TypingTransformers, Transform}
import tools.nsc.ast.TreeDSL
import net.virtualvoid.string.MyNodePrinter
import tools.nsc.symtab.Flags

/**
 * @author il
 */

class GenerateGuiceClass(plugin: AutoGuicePlugin) extends PluginComponent with PluginComponentCommon with Transform with TypingTransformers with TreeDSL with MyNodePrinter{
  val global = plugin.global
  import global._

  val phaseName = this.getClass.getSimpleName
  val runsAfter = "namer" :: Nil

  protected def newTransformer(unit: CompilationUnit): Transformer = new GenerateGuiceClassTransformer(unit)

  class GenerateGuiceClassTransformer(unit: CompilationUnit) extends TypingTransformer(unit) {
    def isAbstract(defdef: DefDef) = {
      //      defdef.rhs.isEmpty
      defdef.mods.isDeferred
    }

    def generateClassImpl(classDef: ClassDef): ClassDef = {
      generateClassImpl(classDef, classDef.symbol.owner)
    }

    def generateClassImpl(classDef: ClassDef, owner: Symbol): ClassDef = {
      val classSym = classDef.symbol
      val classImplSym: ClassSymbol = owner.newClass(classDef.name.append(implSubfix).toTypeName, classDef.pos.focus)

      classImplSym.flags |= (Flags.PRIVATE | Flags.SYNTHETIC | Flags.IMPLCLASS)
      classImplSym.setInfo(
        ClassInfoType(
          definitions.ObjectClass.tpe :: classSym.tpe :: definitions.ScalaObjectClass.tpe :: Nil,
          new Scope(),
          classImplSym
        )
      )
      owner.info.decls.enter(classImplSym)

      val (valDefs, methodDefs) = (for(defdef @ DefDef(_mods, name, _tparams, _vparamss, _tpt, _rhs) <- classDef.impl.body if isAbstract(defdef)) yield atOwner(classImplSym) {
        val constrParamSym = classImplSym.newValue(classImplSym.pos.focus, name)
        constrParamSym.setInfo(defdef.symbol.tpe.resultType)
        constrParamSym.setFlag(Flags.PRIVATE | Flags.LOCAL | Flags.PARAMACCESSOR)
        classImplSym.info.decls.enter(constrParamSym)

        //        val methodSym = defdef.symbol.cloneSymbol(classImplSym)
        //        methodSym.setFlag(Flags.SYNTHETIC | Flags.ACCESSOR | Flags.PARAMACCESSOR)
        //        methodSym.resetFlag(Flags.DEFERRED | Flags.ABSTRACT)
        //        methodSym.setPos(defdef.pos.focus)
        ////        if(defdef.symbol.isStable) {
        //          methodSym.setFlag(Flags.STABLE)
        ////        }
        //        val constrMethodSym = methodSym

        val constrMethodSym: MethodSymbol = classImplSym.newMethod(name, defdef.pos.focus)

        constrMethodSym.setInfo(defdef.symbol.tpe)
        //        constrMethodSym.setInfo( MethodType(Nil, defdef.symbol.tpe.resultType))
        constrMethodSym.setFlag(Flags.STABLE | Flags.ACCESSOR | Flags.PARAMACCESSOR)
        classImplSym.info.decls.enter(constrParamSym)

        val valDef = (localTyper.typed{ ValDef(constrParamSym) }).asInstanceOf[ValDef]
        val methodDef = localTyper.typed{ DefDef(constrMethodSym, Select(This(classImplSym), name)) }
        //        val valDef = ValDef(constrParamSym)
        //        val methodDef = DefDef(constrMethodSym, Select(This(classImplSym), name))
        (valDef, methodDef)
      }).unzip

      val classImplDef: ClassDef = ClassDef(
        classImplSym,
        NoMods,
        List(valDefs),
        List(List()),
        methodDefs,
        NoPosition
      )

      classImplDef.impl.body.filter{
        case dd: DefDef => dd.symbol.isPrimaryConstructor
        case _ => false
      }.foreach(
        _.symbol.addAnnotation(AnnotationInfo(definitions.getClass("javax.inject.Inject": Name).tpe, Nil, Nil))
      )

      classImplDef
    }

    override def transform(tree: Tree): Tree = {
      val newTree: Tree = tree match {
        //        case cd: ClassDef =>
        //          inform(nodeToString(cd))
        //          cd
        case packageDef @ PackageDef(pid, stats) =>
          val newStats = stats.foldRight(List[Tree]()){
            case (classDef @ ClassDef(modifiers, typeName, tparams, impl), list) if isAutoInjectClass(classDef.symbol) =>
              //              val owner0 = localTyper.context1.enclClass.owner
              //              localTyper.context1.enclClass.owner = packageDef.symbol
              val classImplDef = atOwner(packageDef.symbol) {localTyper.typed {
                val classImpl = generateClassImpl(classDef, packageDef.symbol)
                classImpl
              }
              }
              //              localTyper.context1.enclClass.owner = owner0
              classDef :: classImplDef :: list
            case (t, list) => t :: list
          }

          val newPackageDef = treeCopy.PackageDef(packageDef, pid, newStats)
          global.treeBrowser.browse(newPackageDef)
          newPackageDef
        case _ => tree
      }
      super.transform(newTree)
    }
  }
}