package iron9light.autoguice.plugin

import tools.nsc
import nsc.plugins.{PluginComponent, Plugin}
import nsc.ast.TreeDSL
import nsc.Global
import nsc.symtab.Flags
import net.virtualvoid.string.MyNodePrinter
import nsc.transform.{TypingTransformers, Transform}

/**
 * @author il
 */

class AutoGuicePlugin(val global: Global) extends Plugin {
  val name = "autoguice"
  val description = "support for the @autoinject annotation"

  val components = new GenerateGuiceClass(this) ::
    new AddImplementedByAnnotation(this) :: Nil
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
}

class AddImplementedByAnnotation(val plugin: AutoGuicePlugin) extends PluginComponent with PluginComponentCommon with Transform {
  val global = plugin.global

  import global._
  
  val phaseName = this.getClass.getSimpleName
  
  val runsAfter = "typer" :: Nil
  
  protected def newTransformer(unit: CompilationUnit): Transformer = new AddImplementedByAnnotationTransformer(unit)
  
  def implementedByAnnotation(className: String) = {
    val atp: Type = definitions.getClass("com.google.inject.ImplementedBy").tpe
    val implementedClassName = className + "Impl"
    val valueType: Type = definitions.getClass(implementedClassName).tpe
    val annotationInfo = AnnotationInfo(atp, Nil, ("value": Name, LiteralAnnotArg(Constant(valueType))) :: Nil)
    annotationInfo
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
        case _ => tree
      }
      
      super.transform(newTree)
    }
  }
} 

class GenerateGuiceClass(plugin: AutoGuicePlugin) extends PluginComponent with PluginComponentCommon with Transform with TypingTransformers with TreeDSL with MyNodePrinter{
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
    
    def generateClassImpl(classDef: ClassDef): ClassDef = {
      generateClassImpl(classDef, classDef.symbol.owner)
    }
    
    def generateClassImpl(classDef: ClassDef, owner: Symbol): ClassDef = {
      val classSym = classDef.symbol
      val classImplSym: ClassSymbol = owner.newClass(classDef.name.append("Impl").toTypeName)

      classImplSym.flags |= Flags.PRIVATE
      classImplSym.setInfo(
        ClassInfoType(
          definitions.ObjectClass.tpe :: classSym.tpe :: definitions.ScalaObjectClass.tpe :: Nil,
          new Scope(),
          classImplSym
        )
      )
      owner.info.decls.enter(classImplSym)

      val (valDefs, methodDefs) = (for(defdef @ DefDef(_mods, name, _tparams, _vparamss, _tpt, _rhs) <- classDef.impl.body if isAbstract(defdef)) yield {
        val constrParamSym = classImplSym.newValue(classImplSym.pos.focus, name)
        constrParamSym.setInfo(defdef.symbol.tpe.resultType)
        constrParamSym.setFlag(Flags.PRIVATE | Flags.LOCAL | Flags.PARAMACCESSOR)
        classImplSym.info.decls.enter(constrParamSym)

        val valDef = ValDef(constrParamSym, EmptyTree)

        val constrMethodSym: MethodSymbol = classImplSym.newMethod(name, classImplSym.pos.focus)

        constrMethodSym.setInfo(defdef.symbol.tpe)
        constrMethodSym.setFlag(Flags.STABLE | Flags.ACCESSOR | Flags.PARAMACCESSOR | Flags.METHOD)
        classImplSym.info.decls.enter(constrParamSym)

        val methodDef = DefDef(constrMethodSym, Select(This(classImplSym), name))

        (valDef, methodDef)
      }).unzip

      val classImplDef: ClassDef = ClassDef(
        classImplSym,
        Modifiers(Flags.PRIVATE | Flags.LOCAL | Flags.PARAMACCESSOR),
        List(valDefs),
        List(List()),
        methodDefs,
        NoPosition
      )

//      classImplDef.impl.body.filter{
//        case dd: DefDef if dd.name.toString == "<init>" => true
//        case _ => false
//      }.foreach(
//        _.symbol.addAnnotation(AnnotationInfo(definitions.getClass("javax.inject.Inject": Name).tpe, Nil, Nil))
//      )

      classImplDef
    }

    override def transform(tree: Tree): Tree = {

      val newTree: Tree = tree match {
        case ClassDef(_, name, _, _) if name.toString == "XImpl" =>
          inform(printer(tree))
          tree
        case packageDef @ PackageDef(pid, stats) =>
          val newStats = stats.foldRight(List[Tree]()){
            case (classDef @ ClassDef(modifiers, typeName, tparams, impl), list) if isAutoInjectClass(classDef.symbol) =>
              val owner0 = localTyper.context1.owner
              localTyper.context1.owner = packageDef.symbol
              val classImplDef = localTyper.typed {
                generateClassImpl(classDef, packageDef.symbol)
              }
              localTyper.context1.owner = owner0
              classDef :: classImplDef :: list
            case (t, list) => t :: list
          }

          treeCopy.PackageDef(packageDef, pid, newStats)
        case _ => tree
      }
      super.transform(newTree)
    }
  }
}